#' Extract detected events and writes them to a sound file
#'
#' @description Uses audacity labels, either obtained from a text file created with \code{\link[seewave]{write.audacity}} or an object of class \code{\link[bioacoustics]{threshold_detection}} to extract audio events from the original sound file.
#'
#' @param threshold_detection either class threshold_detection or path to audacity marks
#' @param buffer Buffer in seconds added to before and after the event (default 1). Controls also the detection of overlapping events.
#' @param path where to look up the sound file
#' @return data frame
#' @inheritParams rename_recording
#' @inheritParams split_wave
#' @inheritParams bioacoustics::threshold_detection
#' @importFrom tuneR bind
#' @examples
#' # example code
#' \dontrun{
#'path <- system.file("extdata", "20211220_064253.wav", package = "NocMigR2")
#'TD <- find_events(
#'wav.file = path,
#'audacity = FALSE, # Write audacity labels
#'threshold = 8, # SNR in db
#'min_dur = 20, # min length in ms
#'max_dur = 300, # max length in ms
#'LPF = 5000, # low-pass filter at 500
#'HPF = 1000)
#'
#'extract_events(threshold_detection = TD,
#'path = "PATH TO FILE",
#'format = "wav",
#'LPF = 4000,
#'HPF = 1000,
#'buffer = 1)
#'
#'}
#'
#' @export
#'
extract_events <- function(threshold_detection,
                           buffer = 1,
                           format = c("wav", "mp3"),
                           path,
                           LPF = NULL,
                           HPF = NULL) {

  format <- match.arg(format)

  ## check for existing output and drop a comment if found
  previous_output <- list.files(path, "_extracted.WAV", ignore.case = TRUE)
  if (length(previous_output)) cat("\nExisting files '_extracted.WAV will be overwritten!\n")

  ## get df of interest
  if (is.character(threshold_detection)) {
    df <- update_events(txt = threshold_detection)
    df$filename <- stringr::str_replace(df$filename, "txt", format)
    file <- file.path(path, df[1, "filename"])
  } else if (methods::is(threshold_detection, "threshold_detection")) {
    df <- inspect_events(threshold_detection)
    file <- file.path(path, df[1, "filename"])
  }

  ## get file extension
  ext <- tools::file_ext(file)

  ## if mp3, convert to wav first and
  ## then delete wav after processing
  if (ext == "mp3") {
    clean_wav <- TRUE
    old.file <- file
    audio <- tuneR::readMP3(file)
    file <- stringr::str_replace(file, paste0(".", ext, collapse = ""), ".WAV")
    tuneR::writeWave(audio, filename = file)
    ## update ext
    ext <- tools::file_ext(file)

  } else {
    clean_wav <- FALSE
  }

  ## get metadata of sound file
  meta <- tuneR::readWave(file, header = T)
  length <- meta$samples/meta$sample.rate

  ## convert start_times to seconds
  ## --------------------------------
  if (methods::is(threshold_detection, "threshold_detection")) {
    df[["event"]] <- starting_time2seconds(df[["starting_time"]])
    ## update starting time to correct label
    time_offset <- get_DateTime(df[1, "filename"], path)
    df[["starting_time"]] <- df[["event"]] + time_offset$start
  }

  ## Mark from based on buffer
  ## --------------------------------
  df[["from"]] <- ifelse(df[["event"]] - buffer >= 0,
                         df[["event"]] - buffer,
                         0)

  ## Mark end based on buffer
  ## --------------------------------
  df[["to"]] <- ifelse(df[["event"]] + (df[["event"]]/1000) + buffer < length,
                       df[["event"]] + (df[["event"]]/1000) + buffer,
                       length)

  ## sort events in order of appearance (Why unordered in the first place?)
  ## ---------------------------------------------------------------------------
  df <- df[order(df$from),]

  ## check max amplitude frequency is within desired frequency band
  ## if none remain, skip!
  ## ---------------------------------------------------------------------------
  if (!is.null(LPF) & !is.null(HPF)) {
    ## (binding global variable to please R CMD check)
    freq_max_amp <- NULL
    df <- dplyr::filter(df, freq_max_amp < LPF, freq_max_amp > HPF)
  }

  if (nrow(df) > 0) {
    ## Check if overlaps can be eliminated
    ## ----------------------------------------------------------------------------
    df <- non_overlapping(df)

    ## extract audio --> CHECK OVERLAPPING ISSUES IF ANY
    audio <- lapply(1:nrow(df), function(i) {
      ## read signal
      x <- tuneR::readWave(file, df[i, "from"], df[i, "to"], "seconds")
      #if (!is.null(downsample)) x <- tuneR::downsample(x, downsample)
      #if (mono == TRUE & x@stereo == TRUE) x <- tuneR::mono(x, which = 'both')
      return(x)
    })

    ## clean-up
    if (clean_wav == TRUE) unlink(file)

    ## concatenate sound
    audio <- do.call("bind", audio)
    tuneR::writeWave(audio,
                     stringr::str_replace(file, paste0(".", ext, collapse = ""), "_extracted.WAV"))

    ## write labels for use in Audacity
    ## adjust times for extracted audio file
    ## -------------------------------------
    df.adj <- df

    ## set first from = 0 and to = to - from
    ## -------------------------------------
    df.adj$to[1] <- df.adj$to[1] - df.adj$from[1]
    df.adj$from[1] <- 0

    ## now adjust all others ...
    ## -------------------------------------
    if (nrow(df.adj) > 1) {
      for (i in 2:nrow(df.adj)) {
        df.adj$to[i] <- df.adj$to[i] - df.adj$from[i] + df.adj$to[i - 1]
        df.adj$from[i] <- df.adj$to[i - 1]
      }

    }
    dff <- data.frame(label = df.adj$starting_time, t1 = df.adj$from, t2 = df.adj$to)
    seewave::write.audacity(
      dff,
      filename = stringr::str_replace(file, paste0(".", ext, collapse = ""), "_extracted.txt"))

  }
  ## try to free memory
  x <- gc(verbose = FALSE); rm(x)
  return(df)
}
