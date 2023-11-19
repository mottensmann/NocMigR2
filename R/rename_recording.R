#' Rename recording with a string of the form YYYYMMDD_HHMMSS
#'
#' @description
#' Rename recordings using time-stamps derived from \code{ctime} of the audio file.
#'
#' @details
#' Currently renaming distinguishes two cases:
#'    (1) \code{time_reference = 'first'}: Deriving time based on *ctime* (or *mtime*) of first recording and compute for subsequent recordings based on recording duration.
#'    (2) \code{time_reference = 'each'}: Deriving time based on *ctime* (or *mtime*) of each recording separately
#'
#' In order to figure out the correct strategy for recorders not listed here (or when in doubt), make sure to check file properties carefully and run this function with setting \code{.simulate = TRUE}. The choice of *ctime* vs *mtime* might vary depending on the used computer.
#'
#' @param path
#' Path to folder containing audio files
#' @param time_reference
#' (1) Specify \code{'first'} when all recordings share ctime/mtime of the first recording (e.g. \strong{Olympus LS recorder}). (2) Specify \code{'each'} when recordings have unique ctime/mtime (e.g. \strong{Sony PCM recorder})
#'
#' @param ctime logical. defaults to \code{TRUE}. If \code{FALSE} mtime is used instead.
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
    time_reference = c("first", "each"),
    format = c("wav", "mp3"),
    ctime = TRUE,
    .simulate = FALSE) {

  ## match function arguments
  ## ---------------------------------------------------------------------------
  time_reference <- match.arg(time_reference)
  which_time <- ifelse(isTRUE(ctime), 'ctime', 'mtime')
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
  ## Take ctime/mtime of first recording; label in ascending order of file names
  if (time_reference == "first") {

    ## get ctime/mtime of first audio
    meta <- file.info(file.path(path, records[1]))

    if (which_time == 'ctime') {
      ## ctime refers to start of first recording
      df <- data.frame(old.name = records, seconds = seconds, time = meta[[which_time]])
      if (nrow(df) > 1) {
        ## compute times of recordings 2:N
        for (i in 2:nrow(df)) {
          df[i, "time"] <- df[i - 1, "time"] + df[i - 1, "seconds"]
        }
      }
    } else if (which_time == 'mtime') {
      ## mtime refers to end of last recording
      df <- data.frame(old.name = records, seconds = seconds)
      df[["time"]] <- meta[[which_time]] - df[["seconds"]]
      if (nrow(df) > 1) {
        ## compute times of recordings N-1:1
        for (i in (nrow(df) - 1):1) {
          df[i, "time"] <- df[i + 1, "time"] - df[i, "seconds"]
        }
      }
    }

    ## Sony PCM and alike:
  } else if (time_reference == "each" ) {
    ## get time created of first recording
    meta <- lapply(file.path(path, records), file.info)
    meta <- do.call("rbind", meta)

    df <- data.frame(old.name = records,
                     seconds = seconds,
                     time = meta[[which_time]])
  }

  ## create file names based on ctime/mtime of recordings
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
