#' Performs a cleanup of sound archives
#'
#' @details
#' This function is called once files were analysed using BirdNET-Analyzer. Then empty BirdNET.results files are erased as well as wave files containing no audio
#'
#'
#' @inheritParams BirdNET_results2txt
#' @export
#'
BirdNET_tidyup <- function(path, recursive) {
  ## find results files
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "BirdNET.results.txt",
                                      full.names = T,
                                      recursive = recursive)
  ## identify empty files ...
  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))
  ## erase ...
  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }

  # ## identify empty recordings ...
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)
  wavs.size <- file.size(wavs)

  if (any(wavs.size == 0)) unlink(wavs[wavs.size == 0])
}

#' Performs a cleanup of sound archives
#'
#' @details
#' This function is called once files were analysed using BirdNET-Analyzer. Then empty BirdNET.results files are erased as well as wave files containing no audio
#'
#'
#' @inheritParams BirdNET_results2txt
#' @export
#'
BattyBirdNET_tidyup <- function(path, recursive) {
  ## find results files
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "bat.results.txt",
                                      full.names = T,
                                      recursive = recursive)
  ## identify empty files ...
  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))
  ## erase ...
  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }

  # ## identify empty recordings ...
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)
  wavs.size <- file.size(wavs)

  if (any(wavs.size == 0)) unlink(wavs[wavs.size == 0])
}
