#' Read AudioMoth configuration file
#'
#' Reads and parses an AudioMoth configuration file.
#'
#' @param filename Path to the configuration file to read
#' @return A data frame of matching annotations
#' @export
#' @source https://github.com/edwbaker/SonicScrewdriveR/blob/master/R/audiomoth.R
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' audiomothConfig("./CONFIG.TXT")
#' }
#' @export
#'
audiomothConfig <- function(filename) {
  f <- readLines(filename)
  c <- read.csv(textConnection(sub(":", "|", f)), header = FALSE, sep = "|")
  c[,1] <- trimws(c[,1])
  colnames(c) <- c("Key", "Value")
  return(c)
}



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
  species <- hour <- . <- NA

  # 0.) Check 'BirdNET.results.txt'
  BirdNET.results.files <- list.files(path = path,
                                      pattern = "BirdNET.labels.txt",
                                      full.names = T,
                                      recursive = recursive)


  if (length(BirdNET.results.files >= 1)) {
    # read audacity marks
    if (interactive()) {
      cat("Read BirdNET results:\n")
      df <- pbapply::pblapply(BirdNET.results.files,
                              readr::read_delim,
                              show_col_types = FALSE,
                              col_names = c("label", "date", "time", "x1", "score", "x2")) %>%
        do.call("rbind",.)
    } else {
      df <- lapply(BirdNET.results.files,
                   readr::read_delim,
                   show_col_types = FALSE,
                   col_names = c("label", "date", "time", "x1", "score", "x2")) %>%
        do.call("rbind",.)
    }

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
#' @keywords internal
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
#' @keywords internal
#'
inspect_events <- function(threshold_detection = NULL) {
  threshold_detection[["data"]][["event_data"]][,c("filename", "starting_time", "duration", "freq_max_amp", "snr")]
}

#' Update events: Create threshold_detection from audacity labels
#'
#' @param txt audacity labels
#' @keywords internal
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
#' @importFrom warbleR overlapping_sels
#' @keywords internal
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
#' @keywords internal
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

  if (interactive()) {
    duration <- sum(pbapply::pbsapply(waves, function(i) {
      audio <- tuneR::readWave(i, header = TRUE)
      audio$samples/audio$sample.rate
    }))
  } else {
    duration <- sum(sapply(waves, function(i) {
      audio <- tuneR::readWave(i, header = TRUE)
      audio$samples/audio$sample.rate
    }))
  }


  sample_rate <- tuneR::readWave(waves[1], header = TRUE)$sample.rate

  data.frame(sample_rate = paste(sample_rate, "Hz"),
             duration = dplyr::case_when(
               duration < 60 ~ paste(round(duration, 2), "seconds"),
               duration > 60 & duration < 3600 ~ paste(round(duration/60, 2), "minutes"),
               duration > 3600 ~ paste(round(duration/3600, 2), "hours")))
}

#' Read BirdNET_labels.txt
#'
#' @param path path
#' @param recursive logical
#' @keywords internal
#'
BirdNET_labels2results <- function(path, recursive = FALSE) {

  ## binding for global variables to please checks ...
  . <- NA

  ## read labels and find new detections:
  ## they do not contain a number!
  man_detec <- function(x) {
    ## read labels

    df <- utils::read.table(x, sep = "\t", col.names = c("T1", "T2", "Label"))
    df <- df[!grepl("\\d", df$Label),]

    ## compute datetime
    start <- RecreateDateTime(head = basename(x))

    Records <- data.frame(
      Taxon =  df$Label,
      Detector = 'Manual',
      ID = NA,
      #Date = lubridate::date(BirdNET_results$Start +  BirdNET_results$t1),
      T1 = lubridate::as_datetime(start +  df$T1),
      T2 = lubridate::as_datetime(start +  df$T2),
      Score = NA,
      Verification = "T",
      Correction = NA,
      Quality = NA,
      Comment = NA,
      T0 = lubridate::as_datetime(start),
      File =  x) %>%
      ## replace short names in sophisticated way at some point ...
      dplyr::mutate(Taxon = dplyr::case_when(
        Taxon == "A" ~ "Amsel",
        Taxon == "Bp" ~ "Baumpieper",
        Taxon == "Br" ~ "Blesshuhn",
        Taxon == "Tr" ~ "Teichhuhn",
        Taxon == "Sd" ~ "Singdrossel",
        Taxon == "R" ~ "Rotkehlchen",
        Taxon == "Rd" ~ "Rotdrossel",
        Taxon == "Wz" ~ "Waldkauz",
        TRUE ~ Taxon
      ))
  }

  labels2 <- list.files(path, pattern = "labels2", full.names = TRUE, recursive = recursive)
  if (length(labels2) >= 1) {
    out <- lapply(labels2, man_detec) %>%
      do.call("rbind",.)
  } else {
    out <- data.frame()
  }
  return(out)
}

#' Extract specific event from BirdNET results file
#'
#' @details
#' Prerequisites:
#' (1) Run analyzer.py (same folder specified for --i and --o!)
#' (2) Reshape output using function BirdNET (this package)
#' (3) Ensure wav files, BirdNET_results.txt and BirdNET2.xlsx files share the same path
#'
#' @inheritParams BirdNET_extract
#' @keywords internal
#'
BirdNET_extract2 <- function(path = NULL,
                             taxon = NULL,
                             score = NULL,
                             nmax = NULL,
                             sec = 1,
                             output = NULL,
                             #approx_snr = FALSE,
                             spectro = FALSE,
                             hyperlink = T) {

  ## binding for global variables to please checks ...
  name <- NA

  if (!dir.exists(path)) stop("provide valid path")
  path <- tools::file_path_as_absolute(path)

  ## 1.) Check for BirdNET2.xlsx and load if present
  if (!file.exists(file.path(path, "BirdNET2.xlsx"))) stop("BirdNET2.xlsx not found")
  xlsx <- readxl::read_xlsx(file.path(path, "BirdNET2.xlsx"))

  ## check if BirdNET2.xlsx was already formatted with BirdNET_extract
  ## Attempt by all files unique and containing extracted in file name
  if (!any(duplicated(xlsx$File)) & stringr::str_detect(xlsx$File[1], "extracted")) {
    stop("BirdNET2.xlsx already formatted using BirdNET_extract!. Run again with BirdNET() will override existing data")
  }

  ## 2.) Optional: Subset results to extract
  if (!is.null(taxon)) {
    Taxon = NA
    if (length(taxon) == 1) {
      xlsx <- dplyr::filter(xlsx, Taxon == taxon)
    } else {
      xlsx <- dplyr::filter(xlsx, Taxon %in% taxon)
    }

  }

  if (!is.null(score)) {
    Score = NA
    xlsx <- dplyr::filter(xlsx, Score >= score)
  }

  if (!is.null(nmax)) {
    xlsx <- lapply(unique(xlsx[["Taxon"]]), function(one_taxon) {
      df <- dplyr::filter(xlsx, Taxon == one_taxon)
      if (nrow(df) > nmax) {
        return(df[sample(1:nrow(df), nmax, F),])
      } else {
        return(df)
      }
    })
    xlsx <- do.call("rbind", xlsx)
  }

  ## create folder per taxon
  if (is.null(output)) output <- file.path(path, "extracted")
  if (!dir.exists(output)) dir.create(output, showWarnings = FALSE)

  silent <- sapply(unique(xlsx[["Taxon"]]), function(one_taxon) {
    if (!dir.exists(file.path(output, one_taxon))) {
      dir.create(file.path(output, one_taxon), showWarnings = FALSE)
    }})

  ## 3.) Extract ...
  out <- pbapply::pbsapply(1:nrow(xlsx), function(r) {

    # text file
    txt <- xlsx[[r, "File"]]

    ## find wav file
    # Sun Aug  4 12:27:06 2024 ------------------------------
    # Take file argument and identify corresponding audio file
    if (!file.exists(txt)) {
      stop(txt, "does not exist")
    } else {
      # get extension of text file
      txt_ext <- tools::file_ext(txt)
      # replace by 'WAV'
      wav <- stringr::str_replace(txt, txt_ext, "WAV")
      # strip out 'BirdNET.labels2' from file name
      wav <- stringr::str_remove(wav, '.BirdNET.labels2')
      format <- ".WAV"
    }

    # Sat Aug  3 21:28:08 2024 ------------------------------
    #wav <- stringr::str_replace( xlsx[[r, "File"]], "BirdNET.labels2", "WAV")
    #wav <- stringr::str_replace( xlsx[[r, "File"]], "BirdNET.labels2.txt", "WAV")

    if (!file.exists(wav)) {
      wav <- stringr::str_replace(wav, 'WAV', 'wav')
      format <- ".wav"
    }
    if (!file.exists(wav)) {
      stop(wav, " not found")
    }

    ##  select event time stamps
    t0 <- xlsx[[r, "T0"]]; t1 <- xlsx[[r, "T1"]]; t2 <- xlsx[[r, "T2"]]

    ## create a suitable file name
    name <- file.path(
      output,
      xlsx[r, "Taxon"],
      paste0(
        xlsx[[r, "Taxon"]], "_",
        trimws(stringr::str_replace_all(
          substr(as.character(t1), 1, 19), c(":" = "", "-" = "", " " = "_"))), format))

    ## add buffer around event
    from = as.numeric(difftime(t1, t0, units = "secs")) - sec
    if (from < 0) from <- 0
    to = as.numeric(difftime(t2, t0, units = "secs")) + sec
    if (to > t2) to <- t2

    ## extract event
    event <- tuneR::readWave(
      filename = wav,
      from = from,
      to = to,
      units = "seconds")

    ## export audio segments
    tuneR::writeWave(
      object = event,
      filename = name)

    if (isTRUE(spectro)) {
      ## if stereo average to mon
      if (isTRUE(event@stereo)) {
        event <- tuneR::stereo(tuneR::mono(event, "both"),
                               tuneR::mono(event, "both"))
      }

      # saves plot
      dir.create(file.path(dirname(name), "png"), showWarnings = FALSE)
      grDevices::png(
        file.path(file.path(dirname(name), "png"),
                  stringr::str_replace(basename(name), format, ".png")),
        width = 1200, height = 430, res = 72)

      seewave::spectro(wave = event,
                       wl = 1024,
                       grid = F,
                       ovlp = 90,
                       fastdisp = T,
                       scale = F,
                       flab = "",
                       tlab = "",
                       flim = c(2,8),
                       colbg = "white",
                       main = basename(name),
                       palette = seewave::reverse.gray.colors.2)
      grDevices::dev.off()
    }


    return(name)
  })


  ## put hyperlink in excel sheet?
  if (isTRUE(hyperlink) & isFALSE(spectro)) {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET2.xlsx"))

    ## create hyperlink ... check order?
    wav.link <- out; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)
    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET2.xlsx"), overwrite = TRUE)
  } else if (isTRUE(hyperlink) & isTRUE(spectro))  {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET2.xlsx"))

    ## create hyperlink ... check order?
    wav.link <- out; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)

    ## create hyperlink ... check order?

    png.link <- data.frame(
      png = file.path(file.path(dirname(out), "png"),
                      stringr::str_replace(basename(out), format, ".png")))
    class(png.link$png) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = png.link,
                        startCol = 13,
                        colNames = TRUE,
                        startRow = 1)

    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET2.xlsx"), overwrite = TRUE)
  }

  # if (isTRUE(approx_snr)) {
  #
  #   cat("Compute SNRs ... \n")
  #   SNR <- BirdNET_snr(path = path)
  #   cat("done\n")
  #   SNR <- dplyr::left_join(data.frame(sound.files = basename(out)),
  #                           SNR, by = "sound.files")
  #   SNR <- data.frame(SNR = floor(SNR$SNR))
  #   ## open workbook as it is ...
  #   wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET2.xlsx"))
  #
  #   ## insert in wb ...
  #   openxlsx::writeData(
  #     wb = wb,
  #     sheet = 1,
  #     x = SNR,
  #     startCol = ncol(openxlsx::readWorkbook(file.path(path, "BirdNET2.xlsx"))) + 1,
  #     colNames = TRUE,
  #     startRow = 1)
  #   openxlsx::saveWorkbook(wb, file.path(path, "BirdNET2.xlsx"), overwrite = TRUE)
  # }
}

#' Append columns to existing xslx database
#' @param path file
#' @param data data frame
#' @export
#'
xlsx_append <- function(path, data) {

  ## load xlsx file
  xlsx <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx"))

  ## match rows based on basename of audio files
  xlsx[["File"]] <- basename( xlsx[["File"]])

  ## merge data frames
  df <- dplyr::left_join(xlsx, data, by = c("File" = "file"))

  ## open workbook as it is ...
  wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))

  ## insert in wb ...
  openxlsx::writeData(
    wb = wb,
    sheet = 1,
    x = df[,c("SNR", "ampl_mean", "loudness_mean", "loudness_median", "loudness_sd")],
    startCol = ncol(xlsx) + 1,
    colNames = TRUE,
    startRow = 1)
  openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
  return(df)
}

#' Select samples for validation
#'
#' @param x  numeric vector giving indices
#' @keywords internal
#'
sample_rows <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else if (length(x) %in% 2:10) {
    return(x)
  } else {
    return(sample(x = as.numeric(x), size = 10, replace = F))
  }
}

#' Apply high-pass filtering to extracted sound files
#'
#' @description
#' Filtering currently only implemented for mono wave files
#'
#' @param input_dir directory containing wave files to filter
#' @param output_dir output directory. defaults to file.path(input_dir,"filtered")
#' @param hpf high-pass filtering frequency in Hz
#' @param normalise logical
#' @export
#'
audio_filter <- function(input_dir,
                         output_dir = file.path(input_dir,"filtered"),
                         hpf = 20*1000,
                         normalise = FALSE) {

  ## list wave files
  wave_files <- list.files(input_dir, pattern = ".WAV", ignore.case = T)
  ## check for output_dir
  if (!dir.exists(output_dir)) dir.create(output_dir)

  ## batch ...
  waves.filt <- pbapply::pblapply(wave_files, function(wave_file) {
    wave <- tuneR::readWave(file.path(input_dir, wave_file))
    ## hpf
    wave.filt <- seewave::fir(wave, f = wave@samp.rate, from = hpf, bandpass = TRUE)
    ## convert to wave
    wave.filt <- tuneR::Wave(left = wave.filt, samp.rate = wave@samp.rate, bit = 16)
    ## normalize
    if (isTRUE(normalise)) wave.filt <- tuneR::normalize(wave.filt, unit = '16')
    ## export
    tuneR::writeWave(
      wave.filt,
      filename = file.path(output_dir, remove_decimal_seconds(wave_file, input_dir)))



  })
}

#' Eliminate postfix decimal seconds from file names
#' @param wave_file file name
#' @param input_dir path to file to check file extension
#' @keywords internal
#'
remove_decimal_seconds <- function(wave_file, input_dir) {
  ## get extension
  ext <- tools::file_ext(file.path(input_dir, wave_file))
  ## file name without extension
  name_no_ext <- basename(tools::file_path_sans_ext(file.path(input_dir, wave_file)))
  ## split by '.'
  name_simplified <- stringr::str_split(name_no_ext, "\\.", simplify = T)[,1]
  return(paste0(name_simplified, ".", tolower(ext)))
}

#' Rename file names to YYYYMMDD_HHMMSS format
#' @param input_dir input folder
#' @param pattern file extention to look for
#' @keywords internal
#'
rename2DateTime <- function(input_dir, pattern = ".WAV") {

  ## list files based on pattern
  wave_files <- list.files(input_dir, full.names = T, recursive = T, pattern = pattern)

  # Filter out any files from the folder "extracted"
  wave_files <- wave_files[!grepl("extracted/", wave_files)]

  ## Retrieve date & time
  files_new <- substr(wave_files, nchar(wave_files) - 14 - nchar(pattern), nchar(wave_files))

  ## compare strings
  if (all(identical(basename(wave_files), files_new))) {
    cat("Files already named correctly!")
  } else {
    x <- file.rename(from = file.path(dirname(wave_files), basename(wave_files)),
                     to = file.path(dirname(wave_files), files_new))
  }

}


#' Extract datetime from file name
#'
#' @description
#' Extract date and time from file name, assuming a prefix of the form `YYYYMMDD_HHMMSS`
#'
#' @param x file name
#' @return datetime object
#' @export
#'
get_timestamp <- function(x) {
  lubridate::make_datetime(
    year = as.numeric(substr(basename(x), 1, 4)),
    month = as.numeric(substr(basename(x), 5, 6)),
    day = as.numeric(substr(basename(x), 7, 8)),
    hour = as.numeric(substr(basename(x), 10, 11)),
    min =  as.numeric(substr(basename(x), 12, 13)),
    sec = as.numeric(substr(basename(x), 14, 15)))
}
