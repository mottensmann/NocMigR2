#' Cleanup taxon names to avoid problems when specifying path
#'
#' @description
#' Fix the slash '/' in the taxon name and replace by a minus '-'-
#'
#' @param path path
#' @keywords internal
#' @import openxlsx
#' @import readxl
#'
BirdNET_name_repair <- function(path) {
  xlsx_path <- file.path(path, "BirdNET.xlsx")
  wb <- openxlsx::loadWorkbook(xlsx_path)
  df <- readxl::read_xlsx(xlsx_path)

  check <- dplyr::filter(df, grepl("/", Taxon))

  if (nrow(check) > 1) {
    df[['Taxon']] <- gsub("/", "-", df[['Taxon']], fixed = TRUE)
    openxlsx::writeData(wb,
                        sheet = 1,
                        x = df$Taxon,
                        startCol = which(names(df) == "Taxon"),
                        startRow = 2,
                        colNames = FALSE)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    return(data.frame(old = check[["Taxon"]],
                      new = gsub("/", "-", check[['Taxon']], fixed = TRUE)))
  } else {
    return(data.frame())
  }
}
