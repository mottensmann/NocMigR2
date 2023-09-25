#' Process NocMig session for downstream analysis
#'
#' @param path path to NocMig folders
#' @param folder folder to process
#' @inheritParams rename_recording
#' @inheritParams NocMig_meta
#' @export
#'
NocMig_process <- function(
    path = "E:/NocMig", folder = NULL,
    format = c("WAV", "wav", "mp3", "MP3"),
    ctime = c("first", "each"),
    lat = 52.032090,
    lon = 8.516775) {

  ## 01: check path is valid
  ## ---------------------------------------------------------------------------
  if (!dir.exists(file.path(path, folder))) {
    cat(file.path(path, folder), "not found!\n")
  }
  recorder <- match.arg(recorder)
  time <- match.arg(time)

  ## 02: Rename Recording (not touching files with proper name)
  ## ---------------------------------------------------------------------------
  if (recorder != "AudioMoth") {
    cat("Rename files in", file.path(path, folder), "\n")
    output <- rename_recording(
      path = file.path(path, folder),
      ctime = ctime,
      format = format)

  }

  ## 03: Obtain NocMig header
  ## ---------------------------------------------------------------------------
  cat("Write NocMig head to file", file.path(path, folder, "NocMig_meta.txt"), "\n")
  sink(file.path(path, folder, "NocMig_meta.txt"))
  NocMig_meta(lat = lat, lon = lon, date = lubridate::as_date(min(output$time)))
  sink()
}

# path = "E:/NocMig"
# folder = "NocMig01"
# format = "WAV"
# recorder = "Olympus LS-3"
# time = "mtime"
