#' Processing data analysed using BirdNET-Analyzer
#'
#' @inheritParams BirdNET_results2txt
#' @param meta optional. data.frame with recording metadata
#' @examples
#' \dontrun{BirdNET(path = "Path to files")}
#'
#' @export
#'
BirdNET <- function(path = NULL, recursive = FALSE, meta = NULL) {

  if (!dir.exists(path)) stop("provide valid path")

  ## 1.) Summarise
  ## ---------------------------------------------------------------------------
  ## list files and handle case sensitivity
  ## ---------------------------------------------------------------------------
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)
  ## exclude folder extracted if present
  wavs <- stringr::str_subset(wavs, "extracted", negate = TRUE)
  ## obtain duration of last file to get end of recording period
  last.audio <- tuneR::readWave(max(wavs), header = TRUE)
  # last.audio <- tuneR::readWave(file.path(path, max(wavs)), header = TRUE)
  last.audio <- last.audio$samples/last.audio$sample.rate

  wavs <- sapply(wavs, function(x) {
    out <- stringr::str_split(x, "/")[[1]]
    as.character(out[length(out)])
  })
  times <- RecreateDateTime(wavs)
  from <- min(times)
  to <- max(times) + last.audio
  ## check formatting issue at time 00:00:00 and add one second
  if (nchar(as.character(to)) == 10) to <- to + 1

  ## Recording duration
  ## ---------------------------------------------------------------------------
  cat("Calculate total duration of", length(wavs), "recordings:\n")
  duration <- total_duration(path = path, recursive = recursive)[["duration"]]
  ## ---------------------------------------------------------------------------

  ## 2.) Tweak labels
  ## ---------------------------------------------------------------------------
  BirdNET_results <- BirdNET_results2txt(path = path, recursive = recursive)
  BirdNET_table <- BirdNET_table(path = path, recursive = recursive)

  ## 3.) list records
  ## ---------------------------------------------------------------------------
  Records <- data.frame(
    Taxon =  BirdNET_results$label2,
    Detector = 'BirdNET',
    ID = NA,
    #Date = lubridate::date(BirdNET_results$Start +  BirdNET_results$t1),
    T1 = lubridate::as_datetime(BirdNET_results$Start +  BirdNET_results$t1),
    T2 = lubridate::as_datetime(BirdNET_results$Start +  BirdNET_results$t2),
    Score = BirdNET_results$Score,
    Verification = NA,
    Correction = NA,
    Quality = NA,
    Comment = NA,
    T0 = lubridate::as_datetime(BirdNET_results$Start),
    File =  BirdNET_results$file)

  if (is.null(meta)) {
    out <- list(
      Records = Records,
      #Records.dd = BirdNET_table$records.day,
      #Records.hh = BirdNET_table$records.hour,
      Meta = data.frame(From = from,
                        To = to,
                        Duration = duration))
  } else {

    meta[["From"]] <- from; meta[["To"]] <- to; meta[["Duration"]] <- duration

    out <- list(
      Records = Records,
      #Records.dd = BirdNET_table$records.day,
      #Records.hh = BirdNET_table$records.hour,
      Meta = meta)
  }

  openxlsx::write.xlsx(x = out, file = file.path(path, "BirdNET.xlsx"), overwrite = T)
  cat("Created", file.path(path, "BirdNET.xlsx"), "\n")
  return(out)

}
