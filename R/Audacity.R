#' Replacement for \code{\link[seewave]{read.audacity}} to avoid problems with certain labels containing apostrophes etc.
#'
#' @description
#' Read content of a text file and export in format matching \code{\link[seewave]{read.audacity}}
#'
#'
#' @param file A .txt file
#' @param delim delim
#' @param col_names col_names
#' @importFrom readr read_delim
#' @export
#'
read.audacity <- function(file, delim = '\t', col_names = c("t1", "t2", "label", "Score")) {
  df <- readr::read_delim(file = file,
                          delim = delim,
                          col_names = col_names,
                          show_col_types = FALSE,
                          progress = FALSE)

  df <- rbind(data.frame(file = file, df))
  return(df)
}
