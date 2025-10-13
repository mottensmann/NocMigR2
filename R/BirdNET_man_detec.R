#' Append manually recovered detections to existing xlsx file
#'
#' @details
#' Reads audacity labels from a text file and adds to BirdNET.xlsx
#'
#'
#' @inheritParams BirdNET
#' @inheritParams BirdNET_extract
#' @param recursive logical
#' @export
#'
BirdNET_man_detec <- function(path, spectro = FALSE, recursive = FALSE) {


  ## check for manual detections
  Records <- BirdNET_labels2results(path = path, recursive = recursive)

  if (nrow(Records) >= 1) {
    openxlsx::write.xlsx(x = Records, file = file.path(path, "BirdNET2.xlsx"), overwrite = T)
    if (isTRUE(spectro)) warning("Due to an internal bug Spectro = TRUE is currently not supported!")
    #BirdNET_extract2(path = path, spectro = spectro)
    BirdNET_extract2(path = path, spectro = FALSE)

    ## now append to BirdNET.xlsx and delete BirdNET2.xlsx
    ## read db if existing ...

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db1 <- readxl::read_xlsx(path = file.path(path, "BirdNET.xlsx"), sheet = 1)
    ## drop empty rows
    db1 <- db1[rowSums(is.na(db1)) != ncol(db1),]
    db1_rows <- nrow(db1)

    db2 <- readxl::read_xlsx(path = file.path(path, "BirdNET2.xlsx"), sheet = 1)
    ## drop empty rows
    db2 <- db2[rowSums(is.na(db2)) != ncol(db2),]

    if ("png" %in% names(db1) & !"png" %in% names(db2)) {
      db2[["png"]] <- NA
    }

    if (any(duplicated(db1))) warning("Database contains duplicates!")
    db_new <- try(rbind(db1, db2))
    if (!methods::is(db_new, "try-error")) {
      db_new <- dplyr::bind_rows(db1, db2)
    }


    if (any(duplicated(db_new))) {
      stop("Database already contains entries. Skip")
    }

    ## append db2 to db1
    db1 <- file.path(path, "BirdNET.xlsx")

    ## create hyperlink ... check order?
    class(db2$File) <- "hyperlink"

    if ("png" %in% names(db2)) class(db2$png) <- "hyperlink"

    wb <- openxlsx::loadWorkbook(db1)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = db2,
                        colNames = FALSE,
                        startRow = db1_rows + 2)
    openxlsx::saveWorkbook(wb, file = db1, overwrite = TRUE)
    unlink(file.path(path, "BirdNET2.xlsx"))
  }

}
