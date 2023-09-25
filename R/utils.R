#### Internal functions ####
## ---------------------------------------------------------------------------##

#' Data frame of BirdNET detections
#'
#' @param path path
#'
#' @import magrittr
#' @inheritParams BirdNET_results2txt
#' @keywords internal
#'
BirdNET_table <- function(path = NULL, recursive = FALSE) {
  if (!dir.exists(path)) stop("provide valid path")

  ## binding for global variables to please checks ...
  species <- hour <- NA

  # 0.) Check 'BirdNET.results.txt'
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "BirdNET.labels.txt",
                                      full.names = T,
                                      recursive = recursive)


  if (length(BirdNET.results.files >= 1)) {
    # read audacity marks
    df <- suppressMessages(do.call("rbind",
                                   lapply(BirdNET.results.files,
                                          readr::read_delim,
                                          col_names = c("label", "date", "time", "x1", "score", "x2"))))
    # retrieve species
    df[["species"]] <- sapply(df$label, function(x) {
      x1 <- stringr::str_split(x, pattern = "\t")[[1]]
      x1[length(x1)]
    })
    # retrieve hour
    df[["hour"]] <- lubridate::hour(df$time)

    output <- df

    # count per day
    records.day <- df[,c("species", "date")] %>%
      dplyr::group_by(species, date) %>%
      dplyr::summarise(n = dplyr::n())

    # count per hour
    df <- df[,c("species", "hour")] %>%
      dplyr::group_by(species, hour) %>%
      dplyr::summarise(n = dplyr::n())


    df[["species"]] <- factor(x = df[["species"]],
                              levels = unique(as.character(sort(df$species, decreasing = TRUE))))

    return(list(records.all = output[,c("species", "date", "time")],
                records.day = records.day,
                records.hour = df))

  }
}







#' convert start_time to seconds
#' @param t character vector
#' @keywords internal
#'
starting_time2seconds <- function(t) {
  (as.numeric(substr(t, 1,2)) * 60 * 60) +
    (as.numeric(substr(t, 4,5)) * 60) +
    as.numeric(substr(t, 7,12))
}

#' check if file name is composed of date and time string
#' @param files character vector
#'
has_date_time_name <- function(files) {
  ## check for expected nchar --> expect 15 without file extension
  file_extensions <- tools::file_ext(files)
  files_temp <- stringr::str_remove(files, paste0(".", file_extensions))
  l <- sapply(files_temp, nchar)
  files <- files[l == 15]

  ## check content date
  checks_date <- sapply(files, function(x) is.numeric(as.numeric(substr(x, 1, 8))))
  files <- files[checks_date == TRUE]
  checks_time <- sapply(files, function(x) is.numeric(as.numeric(substr(x, 10, 15))))
  files <- files[checks_time == TRUE]
  return(files)
}

#' condense output of find_events
#' @param threshold_detection
#' object of class threshold_detection (see \code{\link[bioacoustics]{threshold_detection}})
#'
inspect_events <- function(threshold_detection = NULL) {
  threshold_detection[["data"]][["event_data"]][,c("filename", "starting_time", "duration", "freq_max_amp", "snr")]
}

#' Update events: Create threshold_detection from audacity labels
#'
#' @param txt audacity labels
#'
update_events <- function(txt = NULL) {
  df <- seewave::read.audacity(file = txt, format = "base")

  ## transform times to HH:MM:SS.SSS
  ## ask if file has had date_time header
  head <- stringr::str_remove(df$file, ".txt")

  if (all(nchar(head) == 15) &
      is.numeric(as.numeric(substr(head, 1, 8))) &
      is.numeric(as.numeric(substr(head, 10, 15)))) {
    origin <- lubridate::make_datetime(
      year = as.numeric(substr(head, 1, 4)),
      month = as.numeric(substr(head, 5, 6)),
      day = as.numeric(substr(head, 7, 8)),
      hour = as.numeric(substr(head, 10, 11)),
      min =  as.numeric(substr(head, 12, 13)),
      sec = as.numeric(substr(head, 14, 15)))
  } else {
    origin <- lubridate::make_datetime(2000, 01, 01, 0, 0, 0)
  }

  ##
  data.frame(filename = df$file,
             starting_time = origin + df$t1,
             duration = df$t2 - df$t1,
             freq_max_amp = df$f2,
             snr = NA,
             event = df$t1)
}

#' Check for and handle overlapping selections
#' @description Detects overlapping selections and merges them using \code{\link[warbleR]{overlapping_sels}}
#' @param df data frame with event data created by \code{\link[bioacoustics]{threshold_detection}}
#'
non_overlapping <- function(df) {
  ## mimic selec_table of warbler package
  df.warbler <- data.frame(sound.files = df$filename,
                           channel = 1,
                           selec = 1:nrow(df),
                           start = df$from,
                           end = df$to,
                           bottom.freq = NA,
                           top.freq = NA)
  out <- suppressMessages(warbleR::overlapping_sels(df.warbler))
  ## any overlap?
  if (any(!is.na(out$ovlp.sels))) {
    df.new <- do.call("rbind", lapply(stats::na.omit(unique(out$ovlp.sels)), function(x) {
      ## (binding global variables to please R CMD check)
      ovlp.sels <- NULL
      subset <- dplyr::filter(out, ovlp.sels == x)
      subset$start <- min(subset$start)
      subset$end <- max(subset$end)
      subset[1,]
    }))
    ## (binding global variables to please R CMD check)
    ovlp.sels <- NULL
    df.new <- rbind(df.new, dplyr::filter(out, is.na(ovlp.sels)))
    out <- df.new[order(df.new$start),]
  }
  ## format as input again
  df.new <- data.frame(filename = out$sound.files,
                       from = out$start,
                       to = out$end)
  df.new <- dplyr::left_join(df.new, df[,c("starting_time", "event", "from")], by = "from")
}




#' Get date and time from file name of the form `YYYYMMDD_HHMMSS`
#'
#' @param target original recording
#' @param target.path original recording path
#' @return data frame
#'
get_DateTime <- function(target, target.path) {

  ext <- tools::file_ext(target)

  if (ext %in% c("WAV", "wav")) {
    info <- tuneR::readWave(file.path(target.path, target), header = TRUE)
    sec <- info$samples/info$sample.rate
  } else if (ext %in% c("MP3", "mp3")) {
    info <- tuneR::readMP3(file.path(target.path, target))
    sec <- as.numeric(summary(info)[[1]])/info@samp.rate
  }
  start <- lubridate::make_datetime(
    year = as.numeric(substr(target, 1, 4)),
    month = as.numeric(substr(target, 5, 6)),
    day = as.numeric(substr(target, 7, 8)),
    hour = as.numeric(substr(target, 10, 11)),
    min =  as.numeric(substr(target, 12, 13)),
    sec = as.numeric(substr(target, 14, 15)))

  return(data.frame(file = target,
                    path = target.path,
                    txt = paste0(substr(target, 1, 15),".txt"),
                    start = start,
                    end = start + sec,
                    sec = sec))
}
#' RecreateDateTime from file name
#'
#' @param head character
#' @keywords internal
#'
RecreateDateTime <- function(head) {
  if (any(nchar(head) < 15)) stop("head is too short")
  return(
    lubridate::make_datetime(
      year = as.numeric(substr(head, 1, 4)),
      month = as.numeric(substr(head, 5, 6)),
      day = as.numeric(substr(head, 7, 8)),
      hour = as.numeric(substr(head, 10, 11)),
      min = as.numeric(substr(head, 12, 13)),
      sec = as.numeric(substr(head, 14, 15))))
}

#' Compute entire recording length
#'
#' @inheritParams rename_recording
#' @inheritParams BirdNET_results2txt
#' @keywords internal
#'
total_duration <- function(path, format = "WAV", recursive = FALSE) {
  waves <- list.files(path = path, pattern = format, full.names = T, recursive = recursive, ignore.case = TRUE)
  waves <- waves[!stringr::str_detect(waves, paste0("_extracted.", format,""))]
  waves <- waves[!stringr::str_detect(waves, "merged.events.WAV")]
  waves <- stringr::str_subset(waves, "extracted", negate = TRUE)

  duration <- sapply(waves, tuneR::readWave, header = T)

  duration <- sum(sapply(waves, function(i) {
    audio <- tuneR::readWave(i, header = TRUE)
    audio$samples/audio$sample.rate
  }))

  sample_rate <- tuneR::readWave(waves[1], header = TRUE)$sample.rate

  data.frame(sample_rate = paste(sample_rate, "Hz"),
             duration = dplyr::case_when(
               duration < 60 ~ paste(round(duration, 2), "seconds"),
               duration > 60 & duration < 3600 ~ paste(round(duration/60, 2), "minutes"),
               duration > 3600 ~ paste(round(duration/3600, 2), "hours")))
}

