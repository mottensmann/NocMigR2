#' Hide wav files from BirdNET analyze.py
#'
#' @description
#' Temporarily move all wav file that where already processed, i.e. `BirdNET.results.txt` exists to another location to avoid rerunning analysis by `BirdNET_analyzer`.
#' Ideally, this is coded directly in analyze.py or its auxiliary functions defined in utils.py
#'
#' @param path path
#' @param hidden.path path
#'
#' @export
#'
BirdNET_hide <- function(path, hidden.path) {

  ## binding for global variables to please checks ...
  # end <- sec <- start <- sound.files <- . <- NA

  ## list BirdNET.results.txt files
  BirdNET.results <- list.files(path, recursive = T, pattern = "BirdNET.results.txt", full.names = F)

  ## list audio files
  audio <- list.files(path, recursive = T, pattern = ".WAV", ignore.case = T, full.names = F)

  ## check for corresponding BirdNET.results.txt
  audio <- unlist(lapply(audio, function(x) {
    y <- stringr::str_replace(string = x, pattern = tools::file_ext(x), replacement = "BirdNET.results.txt")
    if (y %in% BirdNET.results) {
      return(x)
    } else {
      return(character())
    }
  }))

  if (length(audio > 0)) {
    ## move files and keep a record
    cat("Found", length(audio), " files that were already analysed\n")
    df <-  data.frame(from = file.path(path, audio),
                      to =  file.path(hidden.path, basename(audio)))
    utils::write.table(df, file = file.path(hidden.path, paste0(basename(path), ".txt")))
    silent <- file.rename(from = df$from, to = df$to)
  }
}
# path <- "E:/Audiomoth/NH_20231214_20231224"
# hidden.path <- "E:/Audiomoth/Hide_from_BirdNET"


#' Unhide wav files from BirdNET analyze.py
#' @inheritParams BirdNET_hide
#' @export
#'
BirdNET_unhide <- function(path, hidden.path) {
  df <- utils::read.table(file = file.path(hidden.path, paste0(basename(path), ".txt")))
  silent <- file.rename(from = df$to, to = df$from)
}
