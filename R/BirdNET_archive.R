#' Archive validated records
#' @details
#' (1) Load table containing validated data
#' (2) Copy extracted events for future references to local database, distinguishing between true and false positives and unverified events (e.g., species not of primary interest)
#' (3) Providing NocMig summary if \code{NocMig = TRUE} as hourly totals per species
#' (4) Assembling string denoting hourly counts per species ready for submitting observation lists to ornitho
#'
#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param keep.false keep audio classified as false positive? defaults to FALSE
#' @param db local database (.xlsx) to which verified records are added
#' @param NocMig logical
#' @import magrittr
#' @export
#'
BirdNET_archive <- function(
    BirdNET_results = NULL,
    path2archive = NULL,
    NocMig = TRUE,
    keep.false = FALSE,
    db = NULL) {

  ## binding for global variables to please checks ...
  Verification <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- NA

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## (1) Load from Excel ...
  ## ---------------------------------------------------------------------------
  if (!file.exists(BirdNET_results)) stop(BirdNET_results, "not found")
  data_df <- readxl::read_xlsx(BirdNET_results)
  data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
  parent.folder = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]]

  if (isTRUE(NocMig)) {
    if (!file.exists(file.path(parent.folder, "NocMig_meta.txt"))) {
      stop("NocMig_meta.txt not found. Forgot to run NocMig_meta?")
    } else {
      nocmig.meta.string <- readr::read_delim(
        file = file.path(parent.folder, "NocMig_meta.txt"),
        col_names = F,
        show_col_types = F)
      nocmig.str1 <- (trimws(paste0(nocmig.meta.string[1,], collapse = "")))
      nocmig.str2 <- (trimws(paste0(nocmig.meta.string[2,], collapse = "")))
    }
  }

  ## keep just verified detections
  ## -----------------------------------------------------
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))
  if (isTRUE(NocMig)) {
    ## ignore what is not considered NocMig or NFC --> Local
    if (!all(is.na(df$Comment))) {
      if (any(df$Comment == "Local")) df <- df[-which(df$Comment == "Local"),]
    }
  }
  if (nrow(df) > 0) {
    ## check for multiple events assigned to same individual
    ## -----------------------------------------------------
    df <- df %>% dplyr::mutate(check = dplyr::case_when(
      !is.na(ID) ~ paste0(Taxon, ID),
      TRUE ~ NA
    )) %>%
      dplyr::mutate(Hour = lubridate::hour(T1))
    df$check[is.na(df$check)] <- 1:length(df$check[is.na(df$check)])

    ## save df for later use
    df.backup <- df

    ## keep fist detection if multiple exist
    ## check ID ...
    ## -----------------------------------------------------
    dupls <- which(duplicated(df$check))

    if (length(dupls) >= 1) df <- df[-dupls,]

    ## count events: Total and hourly sum
    ## ---------------------------------------------------------------------------
    out <- df %>%
      dplyr::group_by(Taxon, Hour) %>%
      dplyr::summarise(n = dplyr::n())

    ## By Taxon ...
    out <- lapply(unique(out[["Taxon"]]), function(tx) {
      x <- dplyr::filter(out, Taxon == tx) %>%
        dplyr::mutate(part1.sum = sum(n[Hour %in% 17:23])) %>%
        dplyr::mutate(part2.sum = sum(n[!Hour %in% 17:23])) %>%
        dplyr::mutate(Hour = paste0("0", Hour)) %>%
        dplyr::mutate(Hour = stringr::str_sub(Hour, nchar(Hour) - 1, nchar(Hour)))
      ## compose string for ornitho
      ## -------------------------------------------------------------------------
      part1.string = paste0(x$Hour[x$Hour %in% as.character(17:23)], ":",
                            x$n[x$Hour %in% as.character(17:23)], collapse = ",")
      part2.string = paste0(x$Hour[!x$Hour %in% as.character(17:23)], ":",
                            x$n[!x$Hour %in% as.character(17:23)], collapse = ",")
      return(data.frame(Taxon = tx,
                        sum = x[["part1.sum"]][1] + x[["part2.sum"]][1],
                        sum1 = x[["part1.sum"]][1],
                        str1 = ifelse(part1.string == ":", NA, part1.string),
                        sum2 = x[["part2.sum"]][1],
                        str2 = ifelse(part2.string == ":", NA, part2.string)))

    }) %>%
      do.call("rbind",.)
  } else {
    message("No verified detections in ", BirdNET_results, "!")

    out <- data.frame(Taxon = "NULL",
                      sum = NA,
                      sum1 = NA,
                      str1 = NA,
                      sum2 = NA,
                      str2 = NA)

    ## save df for later use
    df.backup <- df
  }

  ## Transfer files to archive
  ## ---------------------------------------------------------------------------
  folder.df <- data.frame(
    file = data_df[["File"]],
    parent = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]],
    child = sapply(data_df[["File"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    }),
    file.new = NA)

  ## true positives
  ## ---------------------------------------------------------------------------
  folder.df.true <-
    dplyr::filter(folder.df, file %in% df.backup[["File"]]) %>%
    dplyr::mutate(to_path = file.path(file.path(path2archive, "True positives"), child))

  taxa <- unique(data_df$Taxon[data_df$Verification == "T"])

  ## Attempt to create main dir
  ## --------------------------
  dir.create(file.path(path2archive, "True positives"), showWarnings = FALSE)

  ## Attempt to create sub dirs
  ## --------------------------
  for (taxon in taxa) {
    dir.create(file.path(path2archive, "True positives", taxon), showWarnings = FALSE)
  }

  ## copy files to new location
  copy_results <- file.copy(
    from = folder.df.true$file,
    to = folder.df.true$to_path,
    overwrite = FALSE,
    copy.date = TRUE)

  ## erase at previous location
  remove_results <- file.remove(folder.df.true$file[isTRUE(copy_results)])

  folder.df$file.new[which(folder.df$file %in% folder.df.true$file)] <- folder.df.true$to_path

  ## false positives
  ## ---------------------------------------------------------------------------
  if (isTRUE(keep.false)) {
    folder.df.false <-
      dplyr::filter(folder.df, !file %in% df[["File"]]) %>%
      dplyr::mutate(to_path = file.path(file.path(path2archive, "False positives"), child))

    taxa <- unique(data_df$Taxon[data_df$Verification == "F"])
    ## Attempt to create main dir
    ## --------------------------
    dir.create(file.path(path2archive, "False positives"), showWarnings = FALSE)

    ## Attempt to create sub dirs
    ## --------------------------
    for (taxon in taxa) {
      dir.create(file.path(path2archive, "False positives", taxon), showWarnings = FALSE)
    }

    ## copy files to new location
    copy_results <- file.copy(
      from = folder.df.false$file,
      to = folder.df.false$to_path,
      overwrite = FALSE,
      copy.date = TRUE)

    ## erase at previous location
    remove_results <- file.remove(folder.df.false$file[copy_results])

    folder.df$file.new[which(folder.df$file %in% folder.df.false$file)] <- folder.df.false$to_path

  }


  ## Return data frame and export to xlsx
  ## ---------------------------------------------------------------------------

  if (isTRUE(NocMig)) {
    output <- cbind(data.frame(Date = as.character(as.Date(data_meta$From))),
                    out,
                    data.frame(Teilliste1 = nocmig.str1,
                               Teilliste2 = nocmig.str2),
                    data_meta)
  } else {
    output <- cbind(data.frame(Date = as.character(as.Date(data_meta$From))),
                    out,
                    data_meta)
  }

  ## read db if existing ...
  if (file.exists(db)) {
    output2 <- output

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db_old <- readxl::read_xlsx(path = db, sheet = 1)

    if (any(duplicated(db_old))) warning("Database contains duplicates!")

    db_new <- try(rbind(db_old, output2))
    if (!methods::is(db_new, "try-error")) {
      db_new <- dplyr::bind_rows(db_old, output2)
    }

    if (any(duplicated(db_new))) {
      warning("Database already contains entries. Skip duplicates")
      indices <- which(duplicated(db_new))
      output2 <- output2[-(indices - nrow(db_old)),]
    }

    wb <- openxlsx::loadWorkbook(db)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = output2,
                        colNames = FALSE,
                        startRow = nrow(openxlsx::readWorkbook(db)) + 2)
    openxlsx::saveWorkbook(wb, file = db, overwrite = TRUE)
    ## create if not ...
  } else {
    openxlsx::write.xlsx(x = output, file = db)
  }

  ## replace links in BirdNET results file
  hLink <- folder.df$file.new
  class(hLink) <- "hyperlink"

  wb <- openxlsx::loadWorkbook(BirdNET_results)
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = hLink,
                      startCol = 12,
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::saveWorkbook(wb, file = BirdNET_results, overwrite = TRUE)

  return(output)
}
