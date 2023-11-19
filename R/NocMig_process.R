#' Process NocMig session for downstream analysis
#'
#' @param parent.folder path
#' @param child.folder optional. defaults to an emoty string \code{''}
#' @param rename logical. defaults to \code{TRUE}
#' @param segment_length optional numeric value. If specified, function \code{\link{split_wave}} is used to to segmenti long audio files in shorter segments. defaults to \code{NULL}
#' @inheritParams rename_recording
#' @inheritParams NocMig_meta
#' @export
#'
NocMig_process <- function(
    parent.folder = "E:/NocMig",
    child.folder = "",
    format = c("wav", "mp3"),
    rename = TRUE,
    time_reference = c("first", "each"),
    ctime = TRUE,
    lat = 52.032090,
    lon = 8.516775,
    segment_length = NULL) {

  format <- match.arg(format)
  time_reference <- match.arg(time_reference)

  ## ---------------------------------------------------------------------------
  if (!dir.exists(file.path(parent.folder, child.folder))) {
    cat(file.path(parent.folder, child.folder), "not found!\n")
  }

  ## 02: Rename Recording (not touching files with proper name!)
  ## ---------------------------------------------------------------------------
  if (isTRUE(rename)) {
    cat("Rename files in", file.path(parent.folder, child.folder), "\n")
    output <- rename_recording(
      path = file.path(parent.folder, child.folder),
      ctime = ctime,
      time_reference = time_reference,
      format = format)
    output.time <- RecreateDateTime(output$new.name)
  } else {
    output <- list.files(path = file.path(parent.folder, child.folder),
                         pattern = paste0(".", format),
                         full.names = F,
                         ignore.case = T)
    output.time <- RecreateDateTime(output)
  }

  ## 03: Obtain NocMig header
  ## ---------------------------------------------------------------------------
  cat("Write NocMig head to file", file.path(parent.folder, child.folder, "NocMig_meta.txt"), "\n")
  sink(file.path(parent.folder, child.folder, "NocMig_meta.txt"))
  NocMig_meta(lat = lat, lon = lon, date = lubridate::as_date(min(output.time)))
  sink()

  if (!is.null(segment_length) & methods::is(segment_length, "numeric")) {
    if (format == "mp3") {
      stop("Segementing audio files required wave files - mp3 is not supported")
    } else {
      audio.files <- list.files(file.path(parent.folder, child.folder),
                                pattern = format, ignore.case = TRUE)
      lapply(audio.files, split_wave,
             path = file.path(parent.folder, child.folder),
             segment = segment_length, discard_input = T)
    }

  }
}

