#' Cleanup folder: \strong{This will delete all files!}
#'
#' @description
#' Removes all files in a folder
#'
#' @param path path
#' @param cleanup logical. defaults to FALSE. Set to TRUE to acutally remove files!
#' @export
#'
BirdNET_cleanup <- function(path = NULL, cleanup = FALSE) {
  if (isTRUE(cleanup)) {
    files <- list.files(path = path,
                        full.names = TRUE,
                        recursive = TRUE)
    unlink(files)
    subdir <- list.dirs(path, recursive = F)
    unlink(subdir, recursive = T)
  }
}
