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

    return(name)
  })


  ## put hyperlink in excel sheet?
  if (isTRUE(hyperlink)) {

    ## open workbook as it is ...
    wb <- openxlsx2::wb_load(file.path(path, "BirdNET2.xlsx"))
    # Get number of rows/cols in the sheet
    data <- openxlsx2::read_xlsx(file.path(path, "BirdNET2.xlsx"), sheet = 'Records')
    n_rows <- nrow(data) + 1   # +1 for header
    n_cols <- ncol(data)

    ## create hyperlink
    urls_full     <- out
    urls_short    <- basename(out)


    ## insert in wb ...
    wb <- openxlsx2::wb_add_formula(
      wb = wb,
      sheet = "Records",
      dims = openxlsx2::wb_dims(rows = 2:(n_rows), cols = which(names(data) == "File")),
      x = paste0('HYPERLINK("', urls_full, '","', urls_short, '")'),
      array = FALSE)

    openxlsx2::wb_save(wb, file.path(path, "BirdNET2.xlsx"), overwrite = TRUE)
  }
}

#' Append columns to existing xslx database
#' @param path file
#' @param data data frame
#' @inheritParams BirdNET
#' @export
#'
xlsx_append <- function(path, data, model = c('BirdNET v2.4', 'Perch v2')) {

  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    xlsx <- 'BirdNET.xlsx'
  } else if (model == 'Perch v2') {
    xlsx <- 'Perch.xlsx'
  }

  ## load xlsx file
  xlsx <- readxl::read_xlsx(file.path(path, xlsx))

  ## match rows based on basename of audio files
  xlsx[["File"]] <- basename( xlsx[["File"]])

  ## merge data frames
  df <- dplyr::left_join(xlsx, data, by = c("File" = "file"))

  ## open workbook as it is ...
  wb <- openxlsx2::wb_load(file.path(path, xlsx))

  ## insert in wb ...
  wb <- openxlsx2::wb_add_data(
    wb = wb,
    sheet = 1,
    x = df[,c("SNR", "ampl_mean", "loudness_mean", "loudness_median", "loudness_sd")],
    start_col = ncol(xlsx) + 1,
    colNames = TRUE,
    start_row = 1)
  openxlsx2::wb_save(wb, file.path(path, xlsx), overwrite = TRUE)
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

#' Wrapper to call bioacoustics::threshold_detection
#'
#' @description
#' Queries bioacoustics::\code{\link[bioacoustics]{threshold_detection}} to detect events in a audio file and exports labels to explore the file in audacity
#'
#' @param wav.file Audio file. Currently supported are WAV or MP3 files.
#' @param overwrite logical
#' @inheritParams bioacoustics::threshold_detection
#' @param audacity logical. If TRUE export audacity lables as txt file
#' @return list (see \code{\link[bioacoustics]{threshold_detection}})
#' @examples
#' # example code
#'path <- system.file("extdata", "20211220_064253.wav", package = "NocMigR2")
#'TD <- find_events(
#'wav.file = path,
#'audacity = FALSE, # Write audacity labels
#'threshold = 8, # SNR in db
#'min_dur = 20, # min length in ms
#'max_dur = 300, # max length in ms
#'LPF = 5000, # low-pass filter at 500
#'HPF = 1000)
#' @export
#'
find_events <- function(wav.file = NULL,
                        audacity = FALSE,
                        overwrite = TRUE,
                        threshold = 10,
                        min_dur = 10,
                        max_dur = 5000,
                        min_TBE = 40,
                        max_TBE = Inf,
                        LPF = 15000,
                        HPF = 80,
                        FFT_size = 1024,
                        start_thr = 20,
                        end_thr = 48,
                        SNR_thr = 4,
                        angle_thr = 125,
                        NWS = 1500,
                        time_scale = 2) {

  ## get file extension
  file_format <- tools::file_ext(wav.file)

  ## check if execution is needed
  check <- TRUE

  if (overwrite == FALSE) {
    txt <- paste0(substr(wav.file, 1, nchar(wav.file) - nchar(tools::file_ext(wav.file))),"txt")
    if (file.exists(txt)) {
      cat(txt, "already exists. Skip recording\n")
      check <- FALSE
    }
  }

  if (check == TRUE) {
    TD <- suppressWarnings(suppressMessages(bioacoustics::threshold_detection(
      wave = wav.file,
      threshold = threshold,
      min_dur = min_dur,
      max_dur = max_dur,
      min_TBE = min_TBE,
      max_TBE = max_TBE,
      time_exp = 1,
      LPF = LPF,
      HPF = HPF,
      start_thr = start_thr,
      end_thr = end_thr,
      SNR_thr = SNR_thr,
      angle_thr = angle_thr,
      NWS = NWS,
      settings = TRUE,
      acoustic_feat = TRUE,
      metadata = FALSE,
      spectro_dir = NULL,
      time_scale = 2,
      ticks = TRUE)))

    if (!is.null(TD$data) & isTRUE(audacity)) {
      ## write audacity marks based on events: times in seconds

      ## ask if file has had date_time header for pretty labels
      head <- stringr::str_remove(TD$data$event_data$filename, paste0(".", file_format))

      if (all(nchar(head) == 15) &
          is.numeric(as.numeric(substr(head, 1, 8))) &
          is.numeric(as.numeric(substr(head, 10, 15)))) {
        origin <- lubridate::make_datetime(
          year = as.numeric(substr(head, 1, 4)),
          month = as.numeric(substr(head, 5, 6)),
          day = as.numeric(substr(head, 7, 8)),
          hour = as.numeric(substr(head, 10, 11)),
          min = as.numeric(substr(head, 12, 13)),
          sec = as.numeric(substr(head, 14, 15)))
      } else {
        origin <- lubridate::make_datetime(2000, 01, 01, 0, 0, 0)
      }

      t <- TD$data$event_data$starting_time
      t1 <-
        (as.numeric(substr(t, 1,2)) * 60 * 60) +
        (as.numeric(substr(t, 4,5)) * 60) +
        as.numeric(substr(t, 7,12))
      t2 <- t1 + (TD$data$event_data$duration / 1000)
      f1 <- TD$data$event_data$freq_center
      f2 <- TD$data$event_data$freq_max_amp
      label <- origin + t1
      dff <- data.frame(label, t1, t2, f1, f2)
      seewave::write.audacity(
        dff,
        filename = paste0(substr(
          wav.file, 1, nchar(wav.file) - nchar(tools::file_ext(wav.file))),"txt"))
    }
    ## try to free memory
    x <- gc(verbose = FALSE); rm(x)
    return(TD)
  }
}

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
  if (methods::is(threshold_detection, "threshold_detection")) {
    df[["event"]] <- starting_time2seconds(df[["starting_time"]])
    ## update starting time to correct label
    time_offset <- get_DateTime(df[1, "filename"], path)
    df[["starting_time"]] <- df[["event"]] + time_offset$start
  }

  ## Mark from based on buffer
  df[["from"]] <- ifelse(df[["event"]] - buffer >= 0,
                         df[["event"]] - buffer,
                         0)

  ## Mark end based on buffer
  df[["to"]] <- ifelse(df[["event"]] + (df[["event"]]/1000) + buffer < length,
                       df[["event"]] + (df[["event"]]/1000) + buffer,
                       length)

  ## sort events in order of appearance (Why unordered in the first place?)
  df <- df[order(df$from),]

  ## check max amplitude frequency is within desired frequency band
  ## if none remain, skip!
  if (!is.null(LPF) & !is.null(HPF)) {
    ## (binding global variable to please R CMD check)
    freq_max_amp <- NULL
    df <- dplyr::filter(df, freq_max_amp < LPF, freq_max_amp > HPF)
  }

  if (nrow(df) > 0) {
    ## Check if overlaps can be eliminated
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
    df.adj <- df

    ## set first from = 0 and to = to - from
    df.adj$to[1] <- df.adj$to[1] - df.adj$from[1]
    df.adj$from[1] <- 0

    ## now adjust all others ...
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

#' Rename recording with a string of the form YYYYMMDD_HHMMSS
#'
#' @description
#' Rename recordings using time-stamps derived from \code{ctime} of the audio file.
#'
#' @details
#' Currently renaming distinguishes two cases:
#'    (1) \code{time_reference = 'first'}: Deriving time based on *ctime* (or *mtime*) of first recording and compute for subsequent recordings based on recording duration.
#'    (2) \code{time_reference = 'each'}: Deriving time based on *ctime* (or *mtime*) of each recording separately
#'
#' In order to figure out the correct strategy for recorders not listed here (or when in doubt), make sure to check file properties carefully and run this function with setting \code{.simulate = TRUE}. The choice of *ctime* vs *mtime* might vary depending on the used computer.
#'
#' @param path
#' Path to folder containing audio files
#' @param time_reference
#' (1) Specify \code{'first'} when all recordings share ctime/mtime of the first recording (e.g. \strong{Olympus LS recorder}). (2) Specify \code{'each'} when recordings have unique ctime/mtime (e.g. \strong{Sony PCM recorder})
#'
#' @param ctime logical. defaults to \code{TRUE}. If \code{FALSE} mtime is used instead.
#'
#' @param format
#' audio format. Supported are \code{'wav'} and \code{'mp3'}
#' @param .simulate
#'  Logical. Default \code{FALSE}
#' @return TRUE if successful
#' @examples
#' # example code
#' rename_recording(
#' path = system.file("extdata", package = "NocMigR2"),
#' format = "wav",
#' .simulate = TRUE)
#'
#' @export
#'
rename_recording <- function(
    path = NULL,
    time_reference = c("first", "each"),
    format = c("wav", "mp3"),
    ctime = TRUE,
    .simulate = FALSE) {

  ## match function arguments
  time_reference <- match.arg(time_reference)
  which_time <- ifelse(isTRUE(ctime), 'ctime', 'mtime')
  format <- match.arg(format)

  ## list recordings
  records <- list.files(path = path, pattern = format, ignore.case = TRUE)
  if (length(records) == 0) stop("No ", format, " files found at ", path)

  ## get duration of recordings
  seconds <- sapply(file.path(path, records), function(x) {
    ## check format
    if (format == "wav") {
      y <- tuneR::readWave(x, header = TRUE)
      y[["samples"]]/y[["sample.rate"]]
    } else if (format == "mp3") {
      y <- tuneR::readMP3(x)
      as.numeric(summary(y)[[1]])/y@samp.rate
    }})

  ## Select option to handle times
  ## Olympus LS  and alike:
  ## Take ctime/mtime of first recording; label in ascending order of file names
  if (time_reference == "first") {

    ## get ctime/mtime of first audio
    meta <- file.info(file.path(path, records[1]))

    if (which_time == 'ctime') {
      ## ctime refers to start of first recording
      df <- data.frame(old.name = records, seconds = seconds, time = meta[[which_time]])
      if (nrow(df) > 1) {
        ## compute times of recordings 2:N
        for (i in 2:nrow(df)) {
          df[i, "time"] <- df[i - 1, "time"] + df[i - 1, "seconds"]
        }
      }
    } else if (which_time == 'mtime') {
      ## mtime refers to end of last recording
      df <- data.frame(old.name = records, seconds = seconds)
      df[["time"]] <- meta[[which_time]] - df[["seconds"]]
      if (nrow(df) > 1) {
        ## compute times of recordings N-1:1
        for (i in (nrow(df) - 1):1) {
          df[i, "time"] <- df[i + 1, "time"] - df[i, "seconds"]
        }
      }
    }

    ## Sony PCM and alike:
  } else if (time_reference == "each" ) {
    ## get time created of first recording
    meta <- lapply(file.path(path, records), file.info)
    meta <- do.call("rbind", meta)

    df <- data.frame(old.name = records,
                     seconds = seconds,
                     time = meta[[which_time]])
  }

  ## create file names based on ctime/mtime of recordings
  df[["new.name"]] <- paste0(substr(df[["time"]], 1, 4),
                             substr(df[["time"]], 6, 7),
                             substr(df[["time"]], 9,10),
                             "_",
                             substr(df[["time"]], 12, 13),
                             substr(df[["time"]], 15, 16),
                             substr(df[["time"]], 18, 19),
                             ".", format)
  rownames(df) <- NULL

  if (isTRUE(.simulate)) {
    #cat("Only show how file.rename will change files!")
  } else {
    ## check:
    if (any(duplicated(df$new.name))) stop("Conflict: Identical file names created. Stop.")

    file.rename(from = file.path(path, records),
                to = file.path(path, df[["new.name"]]))
  }

  return(df[c("old.name", "new.name")])

}

#' Simple function to split large audio into segments
#'
#' @description
#' Cuts audio file into segments for processing
#'
#' @details
#' Optional parameters \code{downsample}, \code{mono} & \code{rescale} require  \code{\link[reticulate]{source_python}} and several python libraries (\code{source_python}, \code{audioop} & \code{wave}).
#'
#' @param path path.
#' @param file file name.
#' @param segment segment length in seconds. Default 600.
#' @param downsample optional. allows to downsample to a new sampling rate in Hz.
#' @param mono logical. By default coerces to mono.
#' @param rescale optional. allows to resacale the wav to a new bit rate (e.g., "8", "16", "24").
#' @param discard_input logical. Allows to discard input file after transformation. Defaults to \code{FALSE}
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
split_wave <- function(
    path = NULL,
    file = NULL,
    segment = 600,
    downsample = NULL,
    mono = TRUE,
    rescale = NULL,
    discard_input = FALSE) {


  #format <- match.arg(format)
  ## define file name
  wave_file <- file.path(path, file)

  ## rename wave_file before processing to avoid issues when segements are saved
  ## in the same folder --> rename afterwards
  wave_file.copy <-
    stringr::str_replace(wave_file,
                         paste0(".", tools::file_ext(wave_file)),
                         paste0(".copy.", tools::file_ext(wave_file)))

  check.rename <- file.rename(from = wave_file,
                              to = wave_file.copy)

  #### downsample ####
  if (!is.null(downsample)) {
    ## check if reticulate is available
    rlang::check_installed("reticulate", reason = "to use `split_wave()`")
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

    #### mono conversion ####
    if (mono == TRUE) {
      check <- resample_wave_mono(wave_file.copy, new_wave_file, downsample)
    } else {
      check <- resample_wave_stereo(wave_file.copy, new_wave_file, downsample)
    }
    ## check that scripts produced output
    wave_file.copy <- new_wave_file
    if (check == FALSE) stop("Python error")
    cat("done\n")
  }

  #### read header of wave file ####
  audio <- tuneR::readWave(filename = wave_file.copy, header = TRUE)
  ## estimate length in seconds
  sec <- audio$samples / audio$sample.rate
  ## define breaks to write audio chunks (keep unique) if
  ## last is identical to duration
  breaks <- unique(c(seq(from = 0, to = sec, by = segment), sec))

  ## get time from file name
  meta <- get_DateTime(target = basename(wave_file.copy), target.path = path)

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

  if (isFALSE(discard_input)) {
    ## create subfolder `split`
    if (!dir.exists(file.path(path, "split"))) dir.create(file.path(path, "split"))
    subfolder <- file.path(path, "split")
  } else if (isTRUE(discard_input)) {
    subfolder <- path
  }

  ## save memory
  #  rm(list = c("audio", "meta", "sec", "breaks"))
  cat("Split ... \n")

  #### extract segments #####
  silent <- pbapply::pblapply(1:nrow(df), function(i) {
    ## read audio
    audio <- tuneR::readWave(filename = wave_file.copy,
                             from = df[i, "from"],
                             to = df[i, "to"],
                             units = "seconds")
    suppressWarnings(tuneR::writeWave(audio, filename = file.path(subfolder, df[i, "new.name"])))
    rm(audio)
    gc(full = T, verbose = F)
  })

  if (isTRUE(discard_input)) {
    unlink(wave_file.copy)
  } else if (isFALSE(discard_input)) {
    check.rename <- file.rename(wave_file.copy, wave_file)
  }
}
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


library(fs)

#' Check for invalid RIFF file
#'
#' @param file wave file
#' @keywords internal
#'
check_wav_header <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))

  # Mindestens 44 Byte für Standard-WAV-Header nötig
  header <- readBin(con, "raw", n = 12)

  if (length(header) < 12) {
    return("invalid")
  }

  riff <- rawToChar(header[1:4])
  wave <- rawToChar(header[9:12])

  if (riff != "RIFF" || wave != "WAVE") {
    return("invalid")
  }

  return("ok")
}
