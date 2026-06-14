#' Performs a cleanup of sound archives
#'
#' @details
#' This function is called once files were analysed using BirdNET-Analyzer. Then empty BirdNET.results files are erased as well as wave files containing no audio
#'
#'
#' @inheritParams BirdNET_results2txt
#' @export
#'
BirdNET_tidyup <- function(path, recursive, model = c('BirdNET v2.4', 'Perch v2')) {


  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    my_pattern    <- 'BirdNET.results.txt'
  } else if (model == 'Perch v2') {
    my_pattern <- 'Perch.results.txt'
  }

  ## find results files
  BirdNET.results.files <- list.files(path = path,
                                      pattern = my_pattern,
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
