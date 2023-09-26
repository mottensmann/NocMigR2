#' Try to approximate signal-to-noise (SNR) ratios for BirdNET_results
#'
#' @inheritParams BirdNET_extract
#' @inheritParams warbleR::sig2noise
#' @description
#' Computing SNR using \code{\link[warbleR]{sig2noise}}. As BirdNET gives detections within imprecise 3 sec intervals events are automatically narrowed down using \code{\link[warbleR]{auto_detec}}
#'
#' @export
#'
BirdNET_snr <- function(path = NULL, mar = 0.5) {

  ## binding for global variables to please checks ...
  end <- sec <- start <- sound.files <- . <- NA

  ## 1.) Check for BirdNET.xlsx and load if present
  ## ---------------------------------------------------------------------------
  if (!file.exists(file.path(path, "BirdNET.xlsx"))) stop("BirdNET.xlsx not found")
  xlsx <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx"))

  ## 2.) create  initial selec_table for WarbleR
  selection_table <- data.frame(
    sound.files = basename(xlsx$File),
    path = dirname(xlsx$File),
    selec = 1,
    start = 1,
    end = 3,
    sec = sapply(xlsx$File, function(i) {
      audio <- tuneR::readWave(i, header = TRUE)
      audio$samples/audio$sample.rate}))

  ## 3.) Run warbleR::auto_detec to pinpoint signal within selction
  selection_table2 <- lapply(X = unique(selection_table$path), function(p) {
    warbleR::auto_detec(X = dplyr::filter(selection_table, path == p),
                        path = p,
                        hold.time = .01,
                        bp = c(0.3, 12),
                        pb = FALSE,
                        threshold = 95)
  }) %>%
    do.call("rbind",.) %>%
    dplyr::left_join(., selection_table[,c("sound.files", "sec", "path")], by = "sound.files")

  ## check rec length to avoid rare issues with margins
  ## if sec < end + mar --> skip
  selection_table2 <- dplyr::filter(selection_table2,
                                    end + mar < sec,
                                    start - mar > 0)

  ## 3.) run WarbleR using lapply as only one path is allowed
  selection_table2.SNR <- lapply(X = unique(selection_table2$path), function(p) {
    warbleR::sig2noise(X = dplyr::filter(selection_table2, path == p),
                       mar = .5,
                       path = p,
                       pb = F)
  }) %>%
    do.call("rbind",.)

  ## 4.) Compute mean SNR per file
  # count per day
  SNR <- selection_table2.SNR %>%
    dplyr::group_by(sound.files) %>%
    dplyr::summarise(SNR = stats::median(SNR))

  return(SNR)
}
