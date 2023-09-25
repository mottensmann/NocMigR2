#' Rename recording with a string of the form YYYYMMDD_HHMMSS
#'
#' @description
#' Rename recordings using time-stamps derived from \code{ctime} of the audio file.
#'
#' @details
#' Currently renaming distinguishes two cases:
#'    (1) \code{ctime = 'first'}: Deriving time based on *ctime* of first recording and compute for subsequent recordings based on recording duration.
#'    (2) \code{ctime = 'each'}: Deriving time based on *ctime* of each recording separately
#'
#' In order to figure out the correct strategy for recorders not listed here (or when in doubt), make sure to check file properties carefully and run this function with setting \code{.simulate = TRUE}
#'
#' @param path
#' Path to folder containing audio files
#' @param ctime
#' (1) Specify \code{'first'} when all recordings share ctime of the first recording (e.g. \strong{Olympus LS recorder}). (2) Specify \code{'each'} when recordings have unique ctime (e.g. \strong{Sony PCM recorder})
#'
#' @param format
#' audio format. Supported are \code{'wav'} and \code{'mp3'}
#' @param .simulate
#'  Logical. Default \code{FALSE}
#' @return TRUE if successful
#' @examples
#' # example code
#' rename_recording(
#' path = system.file("extdata", package = "NocMigR2"),
#' format = "wav",
#' .simulate = TRUE)
#'
#' @export
#'
rename_recording <- function(
    path = NULL,
    ctime = c("first", "each"),
    format = c("wav", "mp3"),
    .simulate = FALSE) {

  ## match function arguments
  ## ---------------------------------------------------------------------------
  ctime <- match.arg(ctime)
  format <- match.arg(format)

  ## list recordings
  ## ---------------------------------------------------------------------------
  records <- list.files(path = path, pattern = format, ignore.case = TRUE)
  if (length(records) == 0) stop("No ", format, " files found at ", path)

  ## get duration of recordings
  ## ---------------------------------------------------------------------------
  seconds <- sapply(file.path(path, records), function(x) {
    ## check format
    ## -------------------------------------------------------------------------
    if (format == "wav") {
      y <- tuneR::readWave(x, header = TRUE)
      y[["samples"]]/y[["sample.rate"]]
    } else if (format == "mp3") {
      y <- tuneR::readMP3(x)
      as.numeric(summary(y)[[1]])/y@samp.rate
    }})

  ## Select option to handle times
  ## ---------------------------------------------------------------------------


  ## Olympus LS  and alike:
  ## Take ctime of first recording; label in ascending order of file names
  if (ctime == "first") {

    ## get ctime of first audio
    meta <- file.info(file.path(path, records[1]))
    df <- data.frame(old.name = records, seconds = seconds, time = meta$ctime)
      if (nrow(df) > 1) {
        ## compute times of recordings 2:N
        for (i in 2:nrow(df)) {
          df[i, "time"] <- df[i - 1, "time"] + df[i - 1, "seconds"]}
      }
    ## Sony PCM and alike:
  } else if (ctime == "each" ) {
    ## get time created of first recording
    meta <- lapply(file.path(path, records), file.info)
    meta <- do.call("rbind", meta)

    df <- data.frame(old.name = records,
                     seconds = seconds,
                     time = meta$ctime)
  }

  ## create file names based on ctime of recordings
  df[["new.name"]] <- paste0(substr(df[["time"]], 1, 4),
                             substr(df[["time"]], 6, 7),
                             substr(df[["time"]], 9,10),
                             "_",
                             substr(df[["time"]], 12, 13),
                             substr(df[["time"]], 15, 16),
                             substr(df[["time"]], 18, 19),
                             ".", format)
  rownames(df) <- NULL

  if (isTRUE(.simulate)) {
    #cat("Only show how file.rename will change files!")
  } else {
    ## check:
    if (any(duplicated(df$new.name))) stop("Conflict: Identical file names created. Stop.")

    file.rename(from = file.path(path, records),
                to = file.path(path, df[["new.name"]]))
  }

  return(df[c("old.name", "new.name")])

}
