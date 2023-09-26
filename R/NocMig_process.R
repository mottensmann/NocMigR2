#' Process NocMig session for downstream analysis
#'
#' @param path path to NocMig folders
#' @param folder folder to process
#' @param rename logical. defaults to \code{TRUE}
#' @inheritParams rename_recording
#' @inheritParams NocMig_meta
#' @export
#'
NocMig_process <- function(
    path = "E:/NocMig", folder = NULL,
    format = c("wav", "mp3"),
    rename = TRUE,
    ctime = c("first", "each"),
    lat = 52.032090,
    lon = 8.516775) {

  ## 01: check path is valid
  ## ---------------------------------------------------------------------------
  if (!dir.exists(file.path(path, folder))) {
    cat(file.path(path, folder), "not found!\n")
  }

  ## 02: Rename Recording (not touching files with proper name!)
  ## ---------------------------------------------------------------------------
  if (isTRUE(rename)) {
    cat("Rename files in", file.path(path, folder), "\n")
    output <- rename_recording(
      path = file.path(path, folder),
      ctime = ctime,
      format = format)
    output.time <- RecreateDateTime(output$new.name)
  } else {
    output <- list.files(path = file.path(path, folder),
                         pattern = paste0(".", format),
                         full.names = F,
                         ignore.case = T)
    output.time <- RecreateDateTime(output)
  }

  ## 03: Obtain NocMig header
  ## ---------------------------------------------------------------------------
  cat("Write NocMig head to file", file.path(path, folder, "NocMig_meta.txt"), "\n")
  sink(file.path(path, folder, "NocMig_meta.txt"))
  NocMig_meta(lat = lat, lon = lon, date = lubridate::as_date(min(output.time)))
  sink()
}

