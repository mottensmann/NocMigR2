#' Try to approximate signal-to-noise (SNR) and loudness indices for BirdNET_results
#'
#' @description
#' (1) Computing SNR using \code{\link[warbleR]{sig2noise}}. As BirdNET gives detections within imprecise 3 sec intervals events are automatically narrowed down using \code{\link[warbleR]{auto_detec}}
#' (2) Computing loudness using \code{\link[soundgen]{getLoudness}}
#'
#' @param path folder containing wave files
#' @param cores number of cores
#' @inheritParams warbleR::sig2noise
#' @import magrittr
#' @export
#'
BirdNET_quality <- function(path, cores = parallel::detectCores() - 2 , mar = 0.5) {

  ## binding for global variables to please checks ...
  SNR <- . <- sound.files <- NA

  cat("Analyse", path, "\n")

  ## (1) Analysis using soundgen
  ## root mean square (RMS)
  rms <- suppressMessages(soundgen::getRMS(x = path, normalize = T, reportEvery = NA, cores = cores )[["summary"]])

  ## loudness
  loudness <- suppressMessages(soundgen::getLoudness(x = path, reportEvery = NA, cores = cores, plot = FALSE)[["summary"]])

  ## (2) Analysis using warbleR
  audio <- list.files(path, recursive = F, pattern = ".wav", ignore.case = T, full.names = TRUE)

  ## auto-detection of events
  ad <- suppressWarnings(warbleR::auto_detec(path = path, bp = c(0.3, 8), wl = 1024, threshold = 85, hold.time = 0.25))

  ## SNR computation
  snr <- suppressWarnings(warbleR::sig2noise(X = ad, mar = 1, path = path, wl = 1024))

  ## Compute mean SNR
  snr <- snr %>%
    dplyr::group_by(sound.files) %>%
    dplyr::summarise(SNR = stats::median(SNR))

  ## merge metrices
  df <-
    dplyr::left_join(rms, loudness, by = "file") %>%
    dplyr::left_join(., snr, by = c("file" = "sound.files"))
  return(df)
}



