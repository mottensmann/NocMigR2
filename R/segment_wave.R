#' segment wave files
#'
#' @description
#' Extracts segments of a user defined length from input files. Input files are either moved to a subfolder (\code{keep.input = TRUE}) or deleted after the processing is executed.
#'
#'
#' @param path path
#' @param length length of individuals audio segments in seconds, only accepting multiples of three. Defaults to 30
#'
#' @param keep.input logical. defaults to TRUE
#'
#' @inheritParams NocMig_process
#'
#' @export
#'
segment_wave <- function(path,
                         length = 30,
                         format = c("wav", "mp3"),
                         keep.input = FALSE) {

  ## (1) check parameter values-------------------------------------------------
  if (!dir.exists(path)) {
    cat(path, "not found!\n")
  }
  format <- match.arg(format)

  ## (2) move wave files to subfolder -----------------------------------------
  input <- list.files(path, full.names = F, pattern = paste0(".", format),
                      ignore.case = T, recursive = F)
  dir.create(file.path(path, 'input'), showWarnings = FALSE)
  x <- file.rename(from = file.path(path, input),
                   to = file.path(path, 'input', input))

  ## (3) segment wave files ----------------------------------------------------
  out <- lapply(input, function(x) {
    ##  obtain meta data -------------------------------------------------------
    meta <- get_DateTime(target = basename(x), file.path(path, 'input'))
    ## define segment start points ---------------------------------------------
    breaks <-
      c(seq(from = 0, to = meta[["sec"]], by = length), meta[["sec"]]) %>%
      unique()
    ## define segment end points -----------------------------------------------
    df <- data.frame(time = meta[["start"]],
                     from = breaks[1:(length(breaks) - 1)],
                     to = breaks[-1],
                     seconds = diff(breaks))
    #
    ## create header based on datetime of recording segments -------------------
    if (nrow(df) > 1) {
      for (i in 2:nrow(df)) {
        df[i, "time"] <- df[i - 1, "time"] + df[i - 1, "seconds"]
      }
    }
    ## create file names based on ctime of recordings
    df[["new.name"]] <- paste0(substr(df[["time"]], 1, 4),
                               substr(df[["time"]], 6, 7),
                               substr(df[["time"]], 9,10),
                               "_",
                               substr(df[["time"]], 12, 13),
                               substr(df[["time"]], 15, 16),
                               substr(df[["time"]], 18, 19),
                               paste0(".", format))
    ## run extractions ---------------------------------------------------------
    silent <- pbapply::pblapply(1:nrow(df), function(i) {
      ## read audio
      audio <- tuneR::readWave(filename = file.path(path, 'input',x),
                               from = df[i, "from"],
                               to = df[i, "to"],
                               units = "seconds")
      tuneR::writeWave(audio, filename = file.path(path, df[i, "new.name"]))
    })

})
  ## (4) Remove input files if requested ---------------------------------------
  if (isFALSE(keep.input)) unlink(file.path(path, 'input'), recursive = T)
}
