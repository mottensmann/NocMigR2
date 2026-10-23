#' Archive validated records of PAM recordings
#' @details
#' (1) Load table containing validated data --> xlsx spreadsheet
#' (2) Copy extracted events for future reference to a local database, distinguishing between true and false positives (unverified records are ignored)

#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param db local database (.xlsx) to which verified records are added
#' @export
#'
BirdNET_archive_pam <- function(BirdNET_results, path2archive, db) {

  ## binding for global variables to please checks ...
  Verification <- Comment <- Min_conf <- Overlap <- Sensitivity <- Hz <-
    Sleep <- Rec <- . <- child <- NA

  ## Checks --
  if (!dir.exists(path2archive)) {
    stop("provide valid path2archive")
  } else  {
    path2archive <- tools::file_path_as_absolute(path2archive)
  }

  ## Format results --
  if (!file.exists(BirdNET_results)) {
    stop(BirdNET_results, "not found")
  } else {
    xlsx_cols <- names(readxl::read_xlsx(BirdNET_results, n_max = 0))
    data_df <- suppressWarnings(readxl::read_xlsx(
      path = BirdNET_results,
      col_types = c(
        'text',    #Taxon
        'text',    #Detector
        'text',    #ID
        'date',    #T1
        'date',    #T2
        'numeric', #Score
        'text',    #Verification
        'text',    #Correction
        'text',    #Quality
        'text',    #Comment
        'date',    #T0
        'text'     #File
      ))) |>
      ## select verified detections
      dplyr::filter(Verification %in% c("T", "TRUE", "true postive")) |>
      ## force comments to character
      dplyr::mutate(Comment = as.character(Comment)) |>
      ## select fields
      dplyr::select(c("Taxon", "Detector", "T1", "Score", "Comment", "File"))

    ### Append meta data --
    data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
    data_df <- cbind(data_df, data_meta) |>
      ## format vars
      dplyr::mutate(
        Min_conf    = ifelse(!is.na(Min_conf),    as.numeric(Min_conf),    NA),
        Overlap     = ifelse(!is.na(Overlap),     as.numeric(Overlap),     NA),
        Sensitivity = ifelse(!is.na(Sensitivity), as.numeric(Sensitivity), NA),
        Hz          = ifelse(!is.na(Hz),          as.numeric(Hz),          NA),
        Sleep       = ifelse(!is.na(Sleep),       as.numeric(Sleep),       NA),
        Rec         = ifelse(!is.na(Rec),         as.numeric(Rec),         NA))
  }

  ## Prepare Archive  --
  if (nrow(data_df) > 0) {
    ## Transfer records to archive
    file.df <- data.frame(
      file   = data_df[["File"]],
      parent = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]],
      child  = sapply(data_df[["File"]], function(x) {
        stringr::str_split(x, "extracted")[[1]][[2]]
      })) |>
      dplyr::mutate(file.new = file.path(path2archive, child))

    data_df <- data_df |>
      dplyr::mutate(File = file.df$file.new)

    taxa <- unique(data_df$Taxon)

    ## Attempt to create sub dirs for detected taxa
    for (taxon in taxa) {
      dir.create(file.path(path2archive, taxon), showWarnings = FALSE)
    }

    ## make some checks --
    file.check <- 0
    checks <- sapply(file.df$file, file.exists) |> as.vector()
    if (any(checks == FALSE) & ! interactive()) {
      stop("Files to copy not found")
    } else if (any(checks == FALSE) & interactive()) {
      message("Files not found at ", dirname(file.df$file[1]))
      file.check <- utils::menu(c("Yes", "No"), title = "Do you want to provide a valid path?")
      if (file.check == 1) {
        file.check <- utils::menu(LETTERS[1:10], title = paste("Change drive letter to ...", ""))
      } else {
        stop("Processing stopped")
      }
    }

    if (file.check != 0) {
      message("Changing drive letter of file sources to ", LETTERS[file.check])
      substr(file.df$file, 1, 1) <- LETTERS[file.check]
    }

    ## Copy files to archive --
    copy_results <- file.copy(
      from      = file.df$file,
      to        = file.df$file.new,
      overwrite = FALSE,
      copy.date = TRUE)

    ## Add records to db --
    if (file.exists(db)) {
      db_import <- data_df

      db_df <- readxl::read_xlsx(path = db, sheet = 'Records')
      if (any(duplicated(db_df))) warning("Database contains duplicates!")

      db_merged <- try(rbind(db_df, db_import))
      if (!methods::is(db_merged, "try-error")) {
        db_merged <- dplyr::bind_rows(db_df, db_import)
      }

      if (any(duplicated(db_merged))) {
        warning("Database already contains entries. Skip duplicates")
        indices <- which(duplicated(db_merged))
        db_import <- db_import[-(indices - nrow(db_df)),]
      }

      if (nrow(db_import) > 0) {
        wb <- openxlsx2::wb_load(db)
        wb <- openxlsx2::wb_add_data(wb        = wb,
                                     sheet     = 'Records',
                                     x         = db_import,
                                     col_names = FALSE,
                                     start_row = nrow(openxlsx2::wb_read(db)) + 2)
        openxlsx2::wb_save(wb = wb, file = db, overwrite = TRUE)
      }

      ## create db if not existing --
    } else {
      openxlsx2::write_xlsx(x = db_import, file = db, overwrite = T, sheet = "Records", na = '')
    }

    ## replace links in (old) BirdNET results file --
    wb <- openxlsx2::wb_load(BirdNET_results)

    ## insert in wb ...
    wb <- openxlsx2::wb_add_data(
      wb = wb,
      sheet = "Records",
      x =  file.df$file.new,
      start_col = which(names(data_df) == "File"),
      colNames = FALSE,
      start_row  = 2)
    openxlsx2::wb_save(wb, file = BirdNET_results, overwrite = TRUE)
    return(db_import)

  } else {
    message("No verified detections in ", BirdNET_results, "!")
    output <- data.frame(Taxon = NA,
                         Detector = NA,
                         T1 = NA,
                         Score = NA,
                         Comment = NA,
                         File = NA) %>%
      cbind(., data_meta)
  }
  reformat_db(path = db)
  return(output)
}

#' Archive validated records of AudioMoth recordings
#' @details
#' (1) Load table containing validated data --> xlsx spreadsheet
#' (2) Copy extracted events for future reference to a local database, distinguishing between true and false positives (unverified records are ignored)

#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param keep.false keep audio classified as false positive? defaults to FALSE
#' @param db local database (.xlsx) to which verified records are added
#' @param png logical. defaults to FALSE
#' @import magrittr
#' @export
#'
BirdNET_archive_am <- function(
    BirdNET_results = NULL,
    path2archive = NULL,
    keep.false = FALSE,
    db = NULL,
    png = FALSE) {

  .Deprecated(new = "BirdNET_archive_pam", 'BirdNET_archive_am is deprecated')

  ## binding for global variables to please checks ...
  Verification <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- file.new <- output <- Min_conf <- Overlap <- Overlap <- Hz <- Sleep <- Rec <- Sensitivity <- folder.df <- NA

  if (isTRUE(png)) warning('use of png = TRUE is deprecated!')

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## Load results
  if (!file.exists(BirdNET_results)) stop(BirdNET_results, "not found")

  ## get columns ...
  xlsx_cols <- names(readxl::read_xlsx(BirdNET_results, n_max = 0))

  if ('png' %in% xlsx_cols) {
    data_df <- readxl::read_xlsx(BirdNET_results, col_types = c(
      'text', #Taxon
      'text', #Detector
      'text', #ID
      'date', #T1
      'date', #T2
      'numeric', #Score
      'text', #Verification
      'text', #Correction
      'text', #Quality
      'text', #Comment
      'date', #T0
      'text', #File
      'text'#png
    )) %>%
      dplyr::mutate(Comment = as.character(Comment))
  } else {
    data_df <- readxl::read_xlsx(BirdNET_results, col_types = c(
      'text', #Taxon
      'text', #Detector
      'text', #ID
      'date', #T1
      'date', #T2
      'numeric', #Score
      'text', #Verification
      'text', #Correction
      'text', #Quality
      'text', #Comment
      'date', #T0
      'text' #File
    )) %>%
      dplyr::mutate(Comment = as.character(Comment))
  }

  data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
  parent.folder = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]]

  ## keep just verified detections
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))

  if (nrow(df) > 0) {
    out <- df[,c("Taxon", "Detector", "T1", "Score", "Comment", "File")] %>%
      cbind(., data_meta)
    out <- out %>%
      dplyr::mutate(Min_conf =ifelse(!is.na(Min_conf), as.numeric(Min_conf), NA),
                    Overlap = ifelse(!is.na(Overlap), as.numeric(Overlap), NA),
                    Sensitivity = ifelse(!is.na(Sensitivity), as.numeric(Sensitivity), NA),
                    Hz = ifelse(!is.na(Hz), as.numeric(Hz), NA),
                    Sleep = ifelse(!is.na(Sleep), as.numeric(Sleep), NA),
                    Rec = ifelse(!is.na(Rec), as.numeric(Rec), NA))

    ## Transfer records to archive
    file.df <- data.frame(
      file = out[["File"]],
      parent = stringr::str_split(out[["File"]][1], "extracted")[[1]][[1]],
      child = sapply(out[["File"]], function(x) {
        stringr::str_split(x, "extracted")[[1]][[2]]
      })) %>%
      dplyr::mutate(file.new = file.path(path2archive, child))

    out <- out %>%
      dplyr::mutate(File = file.df$file.new)

    taxa <- unique(out$Taxon)

    ## Attempt to create sub dirs for detected taxa
    for (taxon in taxa) {
      dir.create(file.path(path2archive, taxon), showWarnings = FALSE)
    }

    ## copy files to new location
    ## check file exist first

    file.check <- 0
    checks <- sapply(file.df$file, file.exists) %>% as.vector()
    if (any(checks == FALSE) & ! interactive()) {
      stop("Files to copy not found")
    } else if (any(checks == FALSE) & interactive()) {
      message("Files not found at ", dirname(file.df$file[1]))
      file.check <- utils::menu(c("Yes", "No"), title = "Do you want to provide a valid path?")
      if (file.check == 1) {
        file.check <- utils::menu(LETTERS[1:10], title = paste("Change drive letter to ...", ""))
      } else {
        stop("Processing stopped")
      }
    }

    if (file.check != 0) {
      message("Changing drive letter of file sources to ", LETTERS[file.check])
      substr(file.df$file, 1, 1) <- LETTERS[file.check]
    }

    copy_results <- file.copy(
      from = file.df$file,
      to = file.df$file.new,
      overwrite = FALSE,
      copy.date = TRUE)

    ## read db if existing ...
    if (file.exists(db)) {
      output2 <- out

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

      wb <- openxlsx2::wb_load(db)
      wb <- openxlsx2::wb_add_data(
        wb = wb,
        sheet = 'Records',
        x = output2,
        col_names = FALSE,
        start_row = nrow(openxlsx2::wb_read(db)) + 2
      )
      openxlsx2::wb_save(wb = wb, file = db, overwrite = TRUE)

    } else {
      openxlsx2::write_xlsx(x = out, file = db, overwrite = T, sheet = "Records", na = '')
    }

    ## replace links in BirdNET results file
    wb <- openxlsx2::wb_load(BirdNET_results)

    ## insert in wb ...
    wb <- openxlsx2::wb_add_data(
      wb = wb,
      sheet = "Records",
      x =  file.df$file.new,
      start_col = which(names(data_df) == "File"),
      colNames = FALSE,
      start_row  = 2)


    openxlsx2::wb_save(wb, file = BirdNET_results, overwrite = TRUE)
    #reformat_db(path = db)
    BirdNET_archive_repair(path = db, path2archive = path2archive)
    return(output)

  } else {
    message("No verified detections in ", BirdNET_results, "!")
    output <- data.frame(Taxon = NA,
                         Detector = NA,
                         T1 = NA,
                         Score = NA,
                         Comment = NA,
                         File = NA) %>%
      cbind(., data_meta)
    return(output)
  }
}

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
  Verification <- folder.df <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- NA

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## Read xlsx data base
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
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))
  if (isTRUE(NocMig)) {
    ## ignore what is not considered NocMig or NFC --> Local
    if (!all(is.na(df$Comment))) {
      if (any(df$Comment == "Local", na.rm = T)) df <- df[-which(df$Comment == "Local"),]
    }
  }
  if (nrow(df) > 0) {
    ## check for multiple events assigned to same individual
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
    dupls <- which(duplicated(df$check))

    if (length(dupls) >= 1) df <- df[-dupls,]

    ## count events: Total and hourly sum
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
  folder.df <- data.frame(
    file = data_df[["File"]],
    parent = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]],
    child = sapply(data_df[["File"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    }),
    file.new = NA)

  ## true positives
  folder.df.true <-
    dplyr::filter(folder.df, file %in% df.backup[["File"]]) %>%
    dplyr::mutate(to_path = file.path(file.path(path2archive, "True positives"), child))

  taxa <- unique(data_df$Taxon[data_df$Verification == "T"])

  ## Attempt to create main dir
  dir.create(file.path(path2archive, "True positives"), showWarnings = FALSE)

  ## Attempt to create sub dirs
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
  if (isTRUE(keep.false)) {
    folder.df.false <-
      dplyr::filter(folder.df, !file %in% df[["File"]]) %>%
      dplyr::mutate(to_path = file.path(file.path(path2archive, "False positives"), child))

    taxa <- unique(data_df$Taxon[data_df$Verification == "F"])
    ## Attempt to create main dir
    dir.create(file.path(path2archive, "False positives"), showWarnings = FALSE)

    ## Attempt to create sub dirs
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
    output2 <- out

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

    wb <- openxlsx2::wb_load(db)
    wb <- openxlsx2::wb_add_data(
      wb = wb,
      sheet = 'Records',
      x = output2,
      col_names = FALSE,
      start_row = nrow(openxlsx2::read_xlsx(db)) + 2)

    openxlsx2::wb_save(wb = wb, file = db)
    ## create if not ...
  } else {
    openxlsx2::write_xlsx(x = output, file = db, overwrite = T, , na = '')
  }

  ## replace links in BirdNET results file
  # Check!
  wb <- openxlsx2::wb_add_formula(
    wb = wb,
    sheet = 'Records',
    dims = openxlsx2::wb_dims(rows = 2:(nrow(folder.df) + 1), cols = 12),
    x = paste0('HYPERLINK("', folder.df$file.new, '","', basename(folder.df$file.new), '")'),
    array = FALSE
  )

  openxlsx2::wb_save(wb, file = BirdNET_results)

  return(output)
}

#' Repair database formatting and hyperlinks
#'
#' @description
#' If `path2archive` is specified, hyperlinks to records are recreated using
#' file.path(path2archive, Taxon, basename(File)).
#'
#' @param path database of pam records
#' @param path2archive path to archive audio data
#' @import openxlsx2
#'
#' @export
BirdNET_archive_repair <- function(path, path2archive) {

  Taxon <- File <- NA

  ## checks function call
  if (!file.exists(path)) stop("provide valid path")
  if (!exists('path2archive')) stop("provide path2archive to apply all checks")
  path2archive <- tools::file_path_as_absolute(path2archive)

  wb <- openxlsx2::wb_load(path)

  ## Get data for dimensions --
  data <- openxlsx2::wb_read(
    path,
    show_hyperlinks = TRUE,
    sheet = "Records")
  n_rows <- nrow(data) + 1
  n_cols <- ncol(data)

  if (all(is.na(data[["File"]]))) {
    stop('Colum "File" formated as hyperlink but no cached result.
         Open and save file in Excel or LibreOffice and try again')
  }

  ## Add filter to header row --
  wb <- openxlsx2::wb_add_filter(wb,
                                 sheet = "Records",
                                 rows  = 1,
                                 cols  = 1:n_cols)

  ## Freeze first row --
  wb <- openxlsx2::wb_freeze_pane(wb,
                                  sheet            = "Records",
                                  first_active_row = 2)

  ## recreate links
  data <- data |>
    dplyr::mutate(File = file.path(path2archive, Taxon, basename(File)))

  ## Hyperlink --
  hyperlink_col <- which(names(data) == "File")
  urls_full     <- data[["File"]]
  urls_short    <- basename(urls_full)

  wb <- openxlsx2::wb_add_formula(
    wb, sheet = "Records",
    dims = openxlsx2::wb_dims(rows = 2:n_rows, cols = hyperlink_col),
    x = paste0('HYPERLINK("', urls_full, '","', urls_short, '")'),
    array = FALSE
  )

  ## Auto adjust column width --
  wb <- openxlsx2::wb_set_col_widths(wb = wb, sheet = "Records",
                                     cols = 1:n_cols, widths = "auto")

  ## Add borders to all cells --
  black <- openxlsx2::wb_color(name = "black")

  wb <- openxlsx2::wb_add_border(
    wb, sheet = "Records",
    dims          = openxlsx2::wb_dims(rows = 1:n_rows, cols = 1:n_cols),
    bottom_border = 'thick', bottom_color = black,
    left_border   = 'thin', left_color    = black,
    top_border    = 'thick', top_color    = black,
    right_border  = 'thin', right_color   = black,
    inner_hgrid   = "thin", inner_hcolor  = black,
    inner_vgrid   = "thin", inner_vcolor  = black
  )

  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)

}

#' Check all linked wav files exist in the archive
#'
#' @param data data frame
#' @keywords internal
#'
BirdNET_archive_checks <- function(data) {

  Taxon <- File <- T1 <- Exists <- NA


  data <- data |>
    dplyr::select(c(Taxon, T1, File)) |>
    dplyr::mutate(Exists = file.exists(File))
  if (any(data[["Exists"]] == FALSE)) {
    out <- dplyr::filter(data, Exists == FALSE)
    warning('Some file are missing in the archive. Check output for details')
    return(out)
  } else {
    message('Archive complete. All files found')
  }
}
