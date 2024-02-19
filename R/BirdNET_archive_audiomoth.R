#' Archive validated records of AudioMoth recordings
#' @details
#' (1) Load table containing validated data
#' (2) Copy extracted events for future references to local database, distinguishing between true and false positives and unverified events (e.g., species not of primary interest)

#'
#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param keep.false keep audio classified as false positive? defaults to FALSE
#' @param db local database (.xlsx) to which verified records are added
#' @import magrittr
#' @export
#'
BirdNET_archive_am <- function(
    BirdNET_results = NULL,
    path2archive = NULL,
    keep.false = FALSE,
    db = NULL) {

  ## binding for global variables to please checks ...
  Verification <- mutate <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- file.new <- output <- NA

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## (1) Load from Excel ...
  ## ---------------------------------------------------------------------------
  if (!file.exists(BirdNET_results)) stop(BirdNET_results, "not found")
  data_df <- readxl::read_xlsx(BirdNET_results) %>%
    mutate(Comment = as.character(Comment))
  data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
  parent.folder = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]]

  ## keep just verified detections
  ## -----------------------------------------------------
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))

  if (nrow(df) > 0) {
    out <- df[,c("Taxon", "Detector", "T1", "Score", "Comment", "File", "png")] %>%
      cbind(., data_meta)

  } else {
    message("No verified detections in ", BirdNET_results, "!")

    out <- data.frame(Taxon = NA,
                      Detector = NA,
                      T1 = NA,
                      Score = NA,
                      Comment = NA,
                      File = NA,
                      png = NA) %>%
      cbind(., data_meta)
  }

  ## Transfer files to archive
  ## ---------------------------------------------------------------------------
  file.df <- data.frame(
    file = out[["File"]],
    parent = stringr::str_split(out[["File"]][1], "extracted")[[1]][[1]],
    child = sapply(out[["File"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    })) %>%
    dplyr::mutate(file.new = file.path(path2archive, child))


  png.df <- data.frame(
    file = out[["png"]],
    parent = stringr::str_split(out[["png"]][1], "extracted")[[1]][[1]],
    child = sapply(out[["png"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    })) %>%
    dplyr::mutate(file.new = file.path(path2archive, file.df$child)) %>%
    dplyr::mutate(file.new = stringr::str_replace(file.new, ".WAV", ".png"))

  taxa <- unique(out$Taxon)

  out <- out %>%
    dplyr::mutate(
      File = file.df$file.new,
      png = png.df$file.new
    )

  ## Attempt to create sub dirs
  ## --------------------------
  for (taxon in taxa) {
    dir.create(file.path(path2archive, taxon), showWarnings = FALSE)
  }

  ## copy files to new location
  ## --------------------------
  copy_results <- file.copy(
    from = file.df$file,
    to = file.df$file.new,
    overwrite = FALSE,
    copy.date = TRUE)

  copy_results <- file.copy(
    from = png.df$file,
    to = png.df$file.new,
    overwrite = FALSE,
    copy.date = TRUE)

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

    wb <- openxlsx::loadWorkbook(db)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = output2,
                        colNames = FALSE,
                        startRow = nrow(openxlsx::readWorkbook(db)) + 2)
    openxlsx::saveWorkbook(wb, file = db, overwrite = TRUE)
    out <- out
    ## create if not ...
  } else {
    openxlsx::write.xlsx(x = out, file = db)
  }

  ## read df to add hyperlink
  df <- readxl::read_xlsx(path = db, sheet = 1)

  ## replace links in BirdNET results file
  hLink.wav <- df$File
  class(hLink.wav) <- "hyperlink"

  hLink.png <- df$png
  class(hLink.png) <- "hyperlink"

  wb <- openxlsx::loadWorkbook(db)
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = hLink.wav,
                      startCol = 6,
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = hLink.png,
                      startCol = 7,
                      colNames = FALSE,
                      startRow = 2)

  openxlsx::saveWorkbook(wb, db, overwrite = TRUE)

  return(output)
}
