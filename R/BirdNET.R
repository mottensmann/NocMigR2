#' Processing data analysed using BirdNET-Analyzer
#'
#' @inheritParams BirdNET_results2txt
#' @param meta optional. data.frame with recording metadata
#' @param am_config logical. attempt to read Audiomoth configuration file if TRUE
#' @examples
#' \dontrun{BirdNET(path = "Path to files")}
#' @import magrittr
#' @export
#'
BirdNET <- function(path = NULL, recursive = FALSE, meta = NULL, am_config = FALSE) {

  if (!dir.exists(path)) stop("provide valid path")

  ## 1.) Summarise
  ## ---------------------------------------------------------------------------
  ## list files and handle case sensitivity
  ## ---------------------------------------------------------------------------
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)

  # Sun Jan  7 11:57:26 2024 ------------------------------
  ## check for folder extracted, ignore those files and present a warninng message
  dirs <- list.dirs(path)
  dirs2 <- stringr::str_remove(dirs, pattern = path)
  if (any(dirs2 == 'extracted')) {
    warning("Detected folder", dirs[which(dirs2 == "extracted")], ".Ignore all wave files in this folder!\n")

  }

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

    if (isTRUE(am_config)) {
      if (file.exists(file.path(path, 'CONFIG.txt'))) {
        am <- audiomothConfig(filename = file.path(path, 'CONFIG.txt'))
        meta <- meta %>%
          dplyr::mutate(
            `Device ID` = trimws(am$Value[am$Key == 'Device ID']),
            Hz = trimws(am$Value[am$Key == 'Sample rate (Hz)']),
            Gain = trimws(am$Value[am$Key == 'Gain']),
            Sleep = trimws(am$Value[am$Key == 'Sleep duration (s)']),
            Rec = trimws(am$Value[am$Key == 'Recording duration (s)']),
            Filter = trimws(am$Value[am$Key == 'Filter']))
      } else {
        meta <- meta %>%
          dplyr::mutate(
            `Device ID` = NA,
            Hz = NA,
            Gain = NA,
            Sleep = NA ,
            Rec = NA ,
            Filter = NA)
      }
    }

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
