#' Simple function to split large audio into segments
#'
#' @description
#' Cuts audio file into segments for processing and applies re-sampling and mono conversion if needed. Requires reticulate and pydub (python)
#'
#' @param path path.
#' @param file file name.
#' @param segment segment length in seconds. Default 600.
#' @param downsample optional. allows to downsample to a new sampling rate in Hz.
#' @param mono logical. By default coerces to mono.
#' @param rescale optional. allows to resacale the wav to a new bit rate (e.g., "8", "16", "24").
#' @return none
#' @examples
#' \dontrun{
#' ## create test folder
#' dir.create("test_folder")
#' ## copy example audio
#' file.copy(from = system.file("extdata", "20211220_064253.wav", package = "NocMigR2"),
#'          to = "test_folder/20211220_064253.wav")
#' ## rename
#' x <- rename_recording("test_folder")
#' x$new.name
#' ## split in segments
#' split_wave(file = x$new.name, # audio file
#'            path = "test_folder/", # folder
#'            segment = 3) # cut in 3 sec segments
#' ## show files
#' list.files("test_folder/split/")
#' ## delete folder
#' unlink("test_folder", recursive = T)
#' }
#'
#' @export
#'
split_wave <- function(path = NULL,
                       file = NULL,
                       segment = 600,
                       downsample = NULL,
                       mono = TRUE,
                       rescale = NULL) {

  ## check if reticulate is available
  ## ---------------------------------------------------------------------------
  rlang::check_installed("reticulate", reason = "to use `split_wave()`")
  #format <- match.arg(format)
  ## define file name
  wave_file <- file.path(path, file)

  ## check python libraries are available
  if (interactive()) {
    ## try to load ...
    check.pydub <- try(reticulate::import(module = "pydub", delay_load = TRUE))
    if (class(check.pydub)[1] == "try-error") {
      stop("pydub is missing. Try to install with reticulate::py_install('pydub')\n")
    }
    ## try to load ...
    check.audioop <- try(reticulate::import(module = "audioop", delay_load = TRUE))
    if (class(check.audioop)[1] == "try-error") {
      stop("audioop is missing. Try to install with reticulate::py_install('audioop')\n")
    }
    ## try to load ...
    check.wave <- try(reticulate::import(module = "wave", delay_load = TRUE))
    if (class(check.wave)[1] == "try-error") {
      stop("wave is missing. Try to install with reticulate::py_install('wave')\n")
    }
  }

  if (!is.null(downsample)) {
    ## down sample and save in temp folder
    temp.dir <- file.path(path, "temp")
    if (!dir.exists(temp.dir)) dir.create(temp.dir)
    new_wave_file <- file.path(file.path(path, "temp"), file)

    ## (binding global variables to please R CMD check)
    resample_wave_mono <- resample_wave_stereo <- NULL

    ## make python script available
    reticulate::source_python(
      system.file("python", "resample_wave_mono.py", package = "NocMigR"))
    cat("\nDownsampling of", file,  "to", downsample, "Hz...\t")
    if (mono == TRUE) {
      check <- resample_wave_mono(wave_file, new_wave_file, downsample)
    } else {
      check <- resample_wave_stereo(wave_file, new_wave_file, downsample)
    }
    ## check that scripts produced output
    wave_file <- new_wave_file
    if (check == FALSE) stop("Python error")
    cat("done\n")
  }

  ## read header of wave file
  audio <- tuneR::readWave(filename = wave_file, header = TRUE)
  ## estimate length in seconds
  sec <- audio$samples / audio$sample.rate
  ## define breaks to write audio chunks (keep unique) if
  ## last is identical to duration
  breaks <- unique(c(seq(from = 0, to = sec, by = segment), sec))

  ## get time from file name
  meta <- get_DateTime(target = file, target.path = path)

  ## define segments
  df <- data.frame(ctime = meta$start,
                   from = breaks[1:(length(breaks) - 1)],
                   to = breaks[-1],
                   seconds = diff(breaks))

  ## adjust times for date_time label as header
  if (nrow(df) > 1) {
    for (i in 2:nrow(df)) {
      df[i, "ctime"] <- df[i - 1, "ctime"] + df[i - 1, "seconds"]
    }
  }


  ## create file names based on ctime of recordings
  df[["new.name"]] <- paste0(substr(df[["ctime"]], 1, 4),
                             substr(df[["ctime"]], 6, 7),
                             substr(df[["ctime"]], 9,10),
                             "_",
                             substr(df[["ctime"]], 12, 13),
                             substr(df[["ctime"]], 15, 16),
                             substr(df[["ctime"]], 18, 19),
                             ".wav")

  ## create subfolder `split`
  if (!dir.exists(file.path(path, "split"))) dir.create(file.path(path, "split"))
  subfolder <- file.path(path, "split")

  ## save memory
  rm(list = c("audio", "meta", "sec", "breaks"))
  cat("Split ... \n")

  silent <- pbapply::pblapply(1:nrow(df), function(i) {
    ## read audio
    audio <- tuneR::readWave(filename = wave_file,
                             from = df[i, "from"],
                             to = df[i, "to"],
                             units = "seconds")
    suppressWarnings(tuneR::writeWave(audio, filename = file.path(subfolder, df[i, "new.name"])))
    rm(audio)
    gc(full = T, verbose = F)
  })
}
