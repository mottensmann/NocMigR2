#' Append manually recovered detections to existing xlsx file
#'
#' @inheritParams BirdNET
#' @inheritParams BirdNET_extract
#' @export
#'
BirdNET_man_detec <- function(path, spectro = FALSE) {


  ## check for manual detections
  ## ---------------------------------------------------------------------------
  Records <- BirdNET_labels2results(path = path)

  if (nrow(Records) >= 1) {
    openxlsx::write.xlsx(x = Records, file = file.path(path, "BirdNET2.xlsx"), overwrite = T)
    BirdNET_extract2(path = path, spectro = spectro)

    ## now append to BirdNET.xlsx and delete BirdNET2.xlsx
    ## read db if existing ...

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db1 <- readxl::read_xlsx(path = file.path(path, "BirdNET.xlsx"), sheet = 1)
    db2 <- readxl::read_xlsx(path = file.path(path, "BirdNET2.xlsx"), sheet = 1)

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
    class(db2$png) <- "hyperlink"


    wb <- openxlsx::loadWorkbook(db1)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = db2,
                        colNames = FALSE,
                        startRow = nrow(openxlsx::readWorkbook(db1)) + 2)
    openxlsx::saveWorkbook(wb, file = db1, overwrite = TRUE)
    unlink(file.path(path, "BirdNET2.xlsx"))
  }

}
