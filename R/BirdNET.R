#' Processing data analysed using BirdNET-Analyzer
#'
#' @inheritParams BirdNET_results2txt
#' @param meta optional. data.frame with recording metadata
#' @param am_config logical. attempt to read Audiomoth configuration file if TRUE
#' @param model One of \code{c('BirdNET v2.4', 'Perch v2')}
#' @examples
#' \dontrun{BirdNET(path = "Path to files")}
#' @import magrittr
#' @export
#'
BirdNET <- function(path = NULL, recursive = FALSE, meta = NULL, am_config = FALSE, model = c('BirdNET v2.4', 'Perch v2')) {

  if (!dir.exists(path)) stop("provide valid path")
  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    xlsx <- 'BirdNET.xlsx'
  } else if (model == 'Perch v2') {
    xlsx <- 'Perch.xlsx'
  }

  ## Summarise and tidyup BirdNET results
  ## list files and handle case sensitivity
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)

  ## erase empty files causing trouble downstream ...
  BirdNET_tidyup(path = path, recursive = recursive)

  ## check for directories
  dirs <- list.dirs(path)
  dirs2 <- stringr::str_remove(dirs, pattern = path)
  if (any(dirs2 == 'extracted')) {
    warning("Detected folder", dirs[which(dirs2 == "extracted")], ".Ignore all wave files in this folder!\n")

  }

  ## exclude folder extracted if present
  wavs <- stringr::str_subset(wavs, "extracted", negate = TRUE)

  ## obtain duration of last file to get end of recording period
  last.audio <- tuneR::readWave(max(wavs), header = TRUE)
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

  ## Estimat total recording duration
  message("Calculate total duration of ", length(wavs), " recordings:\n")
  duration <- total_duration(path = path, recursive = recursive)[["duration"]]

  ## 2.) Tweak labels
  BirdNET_results <- BirdNET_results2txt(path = path, recursive = recursive, model = model)
  BirdNET_table <- BirdNET_table(path = path, recursive = recursive, model = model)

  ## 3.) list records
  Records <- data.frame(
    Taxon =  BirdNET_results$label2,
    Detector = dplyr::case_when(model == 'BirdNET v2.4' ~ 'BirdNET',
                                model == 'Perch v2' ~ 'Perch',
                                TRUE ~ model),
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

  ## export to xlsx file
  openxlsx::write.xlsx(x = out, file = file.path(path, xlsx), overwrite = T)

  reformat_xlsx(path = path, model = model)

  message("Created ", file.path(path, xlsx), "\n")
  ## check for slashes in file names and repair
  check <- BirdNET_name_repair(path = path, model = model)
  if (nrow(check) >= 1) {
    warning("Repaired Taxon names containing slahses '/', replaced by '-'.\n", check)
  }
  return(out)
}


#' Archive validated records of AudioMoth recordings
#' @details
#' (1) Load table containing validated data --> xlsx spreadsheet
#' (2) Copy extracted events for future reference to a local database, distinguishing between true and false positives (unverified records are ignored)

#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param keep.false keep audio classified as false positive? defaults to FALSE
#' @param db local database (.xlsx) to which verified records are added
#' @param png logical. defaults to FALSE
#' @import magrittr dplyr
#' @export
#'
BirdNET_archive_am <- function(
    BirdNET_results = NULL,
    path2archive = NULL,
    keep.false = FALSE,
    db = NULL,
    png = FALSE) {

  ## binding for global variables to please checks ...
  Verification <- mutate <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- file.new <- output <- Min_conf <- Overlap <- Overlap <- Hz <- Sleep <- Rec <- Sensitivity <- NA

  if (isTRUE(png)) warning('use of png = TRUE is deprecated!')

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## Load results
  if (!file.exists(BirdNET_results)) stop(BirdNET_results, "not found")

  ## get columns ...
  xlsx_cols <- names(readxl::read_xlsx(BirdNET_results, n_max = 0))

  if ('png' %in% xlsx_cols) {
    data_df <- readxl::read_xlsx(BirdNET_results, col_types = c(
      'text', #Taxon
      'text', #Detector
      'text', #ID
      'date', #T1
      'date', #T2
      'numeric', #Score
      'text', #Verification
      'text', #Correction
      'text', #Quality
      'text', #Comment
      'date', #T0
      'text', #File
      'text'#png
    )) %>%
      dplyr::mutate(Comment = as.character(Comment))
  } else {
    data_df <- readxl::read_xlsx(BirdNET_results, col_types = c(
      'text', #Taxon
      'text', #Detector
      'text', #ID
      'date', #T1
      'date', #T2
      'numeric', #Score
      'text', #Verification
      'text', #Correction
      'text', #Quality
      'text', #Comment
      'date', #T0
      'text' #File
    )) %>%
      dplyr::mutate(Comment = as.character(Comment))
  }

  data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
  parent.folder = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]]

  ## keep just verified detections
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))

  if (nrow(df) > 0) {
    out <- df[,c("Taxon", "Detector", "T1", "Score", "Comment", "File")] %>%
      cbind(., data_meta)
    out <- out %>%
      dplyr::mutate(Min_conf = as.numeric(Min_conf),
                    Overlap = as.numeric(Overlap),
                    Sensitivity = as.numeric(Sensitivity),
                    Hz = as.numeric(Hz),
                    Sleep = as.numeric(Sleep),
                    Rec = as.numeric(Rec))

  } else {
    message("No verified detections in ", BirdNET_results, "!")
    out <- data.frame(Taxon = NA,
                      Detector = NA,
                      T1 = NA,
                      Score = NA,
                      Comment = NA,
                      File = NA) %>%
      cbind(., data_meta)
  }

  ## Transfer records to archive
  file.df <- data.frame(
    file = out[["File"]],
    parent = stringr::str_split(out[["File"]][1], "extracted")[[1]][[1]],
    child = sapply(out[["File"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    })) %>%
    dplyr::mutate(file.new = file.path(path2archive, child))

  out <- out %>%
    dplyr::mutate(File = file.df$file.new)

  taxa <- unique(out$Taxon)

  ## Attempt to create sub dirs for detected taxa
  for (taxon in taxa) {
    dir.create(file.path(path2archive, taxon), showWarnings = FALSE)
  }

  ## copy files to new location
  ## check file exist first

  file.check <- 0
  checks <- sapply(file.df$file, file.exists) %>% as.vector()
  if (any(checks == FALSE) & ! interactive()) {
    stop("Files to copy not found")
  } else if (any(checks == FALSE) & interactive()) {
    cat("Files not found at", dirname(file.df$file[1]), "\n")
    file.check <- utils::menu(c("Yes", "No"), title = "Do you want to provide a valid path?")
    if (file.check == 1) {
      file.check <- utils::menu(LETTERS[1:10], title = paste("Change drive letter to ...", ""))
    } else {
      stop("Processing stopped")
    }
  }

  if (file.check != 0) {
    cat("Changing drive letter of file sources to", LETTERS[file.check])
    substr(file.df$file, 1, 1) <- LETTERS[file.check]
  }

  copy_results <- file.copy(
    from = file.df$file,
    to = file.df$file.new,
    overwrite = FALSE,
    copy.date = TRUE)

  ## read db if existing
  if (file.exists(db)) {
    output2 <- out

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db_head <- readxl::read_xlsx(path = db, sheet = 1, n_max = 1)
    db_old <- suppressWarnings(readxl::read_xlsx(path = db, sheet = 1, col_types = c(
      'text', #Taxon
      'text', #Detector
      'date', #T1
      'numeric', #Score
      'text', #Comment
      'text', #File
      'text', #Location
      'numeric', #Lat
      'numeric', #Lon
      'date', #From
      'date', #To
      'text', #Duration
      'text', #Device
      'text', #Micro
      'numeric', #min_conf
      'numeric', #overlap
      'numeric', #sensitivity
      'text', #slist
      'text', #Device ID
      'numeric',#hz
      'text', #gain
      'numeric',#sleep
      'numeric',#rec
      'text'#filter
    )))

    if (any(duplicated(db_old))) {
      warning("Database contains duplicates!")
      db_old[which(duplicated(db_old)),]
    }

    db_new <- suppressWarnings(try(rbind(db_old, output2), silent = TRUE))
    if (!methods::is(db_new, "try-error")) {
      db_new <- suppressWarnings(dplyr::bind_rows(db_old, output2))
    }

    if (any(duplicated(db_new))) {
      warning("Database already contains entries. Skip duplicates")
      indices <- which(duplicated(db_new))
      output2 <- output2[-(indices - nrow(db_old)),]
    }

    wb <- openxlsx::loadWorkbook(db)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = output2,
                        colNames = FALSE,
                        startRow = nrow(openxlsx::readWorkbook(db)) + 2)
    openxlsx::saveWorkbook(wb, file = db, overwrite = TRUE)
    out <- out
    ## create if not ...
  } else {
    openxlsx::write.xlsx(x = out, file = db)
  }

  ## read df to add hyperlink
  df <- readxl::read_xlsx(path = db, sheet = 1)

  ## replace links in BirdNET results file
  hLink.wav <- df$File
  class(hLink.wav) <- "hyperlink"

  wb <- openxlsx::loadWorkbook(db)
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = hLink.wav,
                      startCol = which(names(db_head) == 'File'),
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::saveWorkbook(wb, db, overwrite = TRUE)
  #reformat_db(path = db)

  return(output)
}

#' Archive validated records
#' @details
#' (1) Load table containing validated data
#' (2) Copy extracted events for future references to local database, distinguishing between true and false positives and unverified events (e.g., species not of primary interest)
#' (3) Providing NocMig summary if \code{NocMig = TRUE} as hourly totals per species
#' (4) Assembling string denoting hourly counts per species ready for submitting observation lists to ornitho
#'
#' @param BirdNET_results path to verified BirdNET.xlsx file
#' @param path2archive path to archive audio data
#' @param keep.false keep audio classified as false positive? defaults to FALSE
#' @param db local database (.xlsx) to which verified records are added
#' @param NocMig logical
#' @import magrittr
#' @export
#'
BirdNET_archive <- function(
    BirdNET_results = NULL,
    path2archive = NULL,
    NocMig = TRUE,
    keep.false = FALSE,
    db = NULL) {

  ## binding for global variables to please checks ...
  Verification <- Comment <- T1 <- Taxon <- Hour <- n <- . <- child <- NA

  if (!dir.exists(path2archive)) stop("provide valid path2archive")
  path2archive <- tools::file_path_as_absolute(path2archive)

  ## Read xlsx data base
  if (!file.exists(BirdNET_results)) stop(BirdNET_results, "not found")
  data_df <- readxl::read_xlsx(BirdNET_results)
  data_meta <- readxl::read_xlsx(BirdNET_results, sheet = "Meta")
  parent.folder = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]]

  if (isTRUE(NocMig)) {
    if (!file.exists(file.path(parent.folder, "NocMig_meta.txt"))) {
      stop("NocMig_meta.txt not found. Forgot to run NocMig_meta?")
    } else {
      nocmig.meta.string <- readr::read_delim(
        file = file.path(parent.folder, "NocMig_meta.txt"),
        col_names = F,
        show_col_types = F)
      nocmig.str1 <- (trimws(paste0(nocmig.meta.string[1,], collapse = "")))
      nocmig.str2 <- (trimws(paste0(nocmig.meta.string[2,], collapse = "")))
    }
  }

  ## keep just verified detections
  df <- dplyr::filter(data_df, Verification %in% c("T", "TRUE", "true postive"))
  if (isTRUE(NocMig)) {
    ## ignore what is not considered NocMig or NFC --> Local
    if (!all(is.na(df$Comment))) {
      if (any(df$Comment == "Local", na.rm = T)) df <- df[-which(df$Comment == "Local"),]
    }
  }
  if (nrow(df) > 0) {
    ## check for multiple events assigned to same individual
    df <- df %>% dplyr::mutate(check = dplyr::case_when(
      !is.na(ID) ~ paste0(Taxon, ID),
      TRUE ~ NA
    )) %>%
      dplyr::mutate(Hour = lubridate::hour(T1))
    df$check[is.na(df$check)] <- 1:length(df$check[is.na(df$check)])

    ## save df for later use
    df.backup <- df

    ## keep fist detection if multiple exist
    ## check ID ...
    dupls <- which(duplicated(df$check))

    if (length(dupls) >= 1) df <- df[-dupls,]

    ## count events: Total and hourly sum
    out <- df %>%
      dplyr::group_by(Taxon, Hour) %>%
      dplyr::summarise(n = dplyr::n())

    ## By Taxon ...
    out <- lapply(unique(out[["Taxon"]]), function(tx) {
      x <- dplyr::filter(out, Taxon == tx) %>%
        dplyr::mutate(part1.sum = sum(n[Hour %in% 17:23])) %>%
        dplyr::mutate(part2.sum = sum(n[!Hour %in% 17:23])) %>%
        dplyr::mutate(Hour = paste0("0", Hour)) %>%
        dplyr::mutate(Hour = stringr::str_sub(Hour, nchar(Hour) - 1, nchar(Hour)))
      ## compose string for ornitho
      part1.string = paste0(x$Hour[x$Hour %in% as.character(17:23)], ":",
                            x$n[x$Hour %in% as.character(17:23)], collapse = ",")
      part2.string = paste0(x$Hour[!x$Hour %in% as.character(17:23)], ":",
                            x$n[!x$Hour %in% as.character(17:23)], collapse = ",")
      return(data.frame(Taxon = tx,
                        sum = x[["part1.sum"]][1] + x[["part2.sum"]][1],
                        sum1 = x[["part1.sum"]][1],
                        str1 = ifelse(part1.string == ":", NA, part1.string),
                        sum2 = x[["part2.sum"]][1],
                        str2 = ifelse(part2.string == ":", NA, part2.string)))

    }) %>%
      do.call("rbind",.)
  } else {
    message("No verified detections in ", BirdNET_results, "!")

    out <- data.frame(Taxon = "NULL",
                      sum = NA,
                      sum1 = NA,
                      str1 = NA,
                      sum2 = NA,
                      str2 = NA)

    ## save df for later use
    df.backup <- df
  }

  ## Transfer files to archive
  folder.df <- data.frame(
    file = data_df[["File"]],
    parent = stringr::str_split(data_df[["File"]][1], "extracted")[[1]][[1]],
    child = sapply(data_df[["File"]], function(x) {
      stringr::str_split(x, "extracted")[[1]][[2]]
    }),
    file.new = NA)

  ## true positives
  folder.df.true <-
    dplyr::filter(folder.df, file %in% df.backup[["File"]]) %>%
    dplyr::mutate(to_path = file.path(file.path(path2archive, "True positives"), child))

  taxa <- unique(data_df$Taxon[data_df$Verification == "T"])

  ## Attempt to create main dir
  dir.create(file.path(path2archive, "True positives"), showWarnings = FALSE)

  ## Attempt to create sub dirs
  for (taxon in taxa) {
    dir.create(file.path(path2archive, "True positives", taxon), showWarnings = FALSE)
  }

  ## copy files to new location
  copy_results <- file.copy(
    from = folder.df.true$file,
    to = folder.df.true$to_path,
    overwrite = FALSE,
    copy.date = TRUE)

  ## erase at previous location
  remove_results <- file.remove(folder.df.true$file[isTRUE(copy_results)])

  folder.df$file.new[which(folder.df$file %in% folder.df.true$file)] <- folder.df.true$to_path

  ## false positives
  if (isTRUE(keep.false)) {
    folder.df.false <-
      dplyr::filter(folder.df, !file %in% df[["File"]]) %>%
      dplyr::mutate(to_path = file.path(file.path(path2archive, "False positives"), child))

    taxa <- unique(data_df$Taxon[data_df$Verification == "F"])
    ## Attempt to create main dir
    dir.create(file.path(path2archive, "False positives"), showWarnings = FALSE)

    ## Attempt to create sub dirs
    for (taxon in taxa) {
      dir.create(file.path(path2archive, "False positives", taxon), showWarnings = FALSE)
    }

    ## copy files to new location
    copy_results <- file.copy(
      from = folder.df.false$file,
      to = folder.df.false$to_path,
      overwrite = FALSE,
      copy.date = TRUE)

    ## erase at previous location
    remove_results <- file.remove(folder.df.false$file[copy_results])

    folder.df$file.new[which(folder.df$file %in% folder.df.false$file)] <- folder.df.false$to_path

  }


  ## Return data frame and export to xlsx
  if (isTRUE(NocMig)) {
    output <- cbind(data.frame(Date = as.character(as.Date(data_meta$From))),
                    out,
                    data.frame(Teilliste1 = nocmig.str1,
                               Teilliste2 = nocmig.str2),
                    data_meta)
  } else {
    output <- cbind(data.frame(Date = as.character(as.Date(data_meta$From))),
                    out,
                    data_meta)
  }

  ## read db if existing ...
  if (file.exists(db)) {
    output2 <- output

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db_old <- readxl::read_xlsx(path = db, sheet = 1)

    if (any(duplicated(db_old))) warning("Database contains duplicates!")

    db_new <- try(rbind(db_old, output2))
    if (!methods::is(db_new, "try-error")) {
      db_new <- dplyr::bind_rows(db_old, output2)
    }

    if (any(duplicated(db_new))) {
      warning("Database already contains entries. Skip duplicates")
      indices <- which(duplicated(db_new))
      output2 <- output2[-(indices - nrow(db_old)),]
    }

    wb <- openxlsx::loadWorkbook(db)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = output2,
                        colNames = FALSE,
                        startRow = nrow(openxlsx::readWorkbook(db)) + 2)
    openxlsx::saveWorkbook(wb, file = db, overwrite = TRUE)
    ## create if not ...
  } else {
    openxlsx::write.xlsx(x = output, file = db)
  }

  ## replace links in BirdNET results file
  hLink <- folder.df$file.new
  class(hLink) <- "hyperlink"

  wb <- openxlsx::loadWorkbook(BirdNET_results)
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x = hLink,
                      startCol = 12,
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::saveWorkbook(wb, file = BirdNET_results, overwrite = TRUE)

  return(output)
}
#' Cleanup folder: \strong{This will delete all files!}
#'
#' @description
#' Removes all files in a folder
#'
#' @param path path
#' @param cleanup logical. defaults to FALSE. Set to TRUE to actually remove files!
#' @export
#'
BirdNET_cleanup <- function(path = NULL, cleanup = FALSE) {
  if (isTRUE(cleanup)) {
    files <- list.files(path = path,
                        full.names = TRUE,
                        recursive = TRUE)
    unlink(files)
    subdir <- list.dirs(path, recursive = F)
    unlink(subdir, recursive = T)
  }
}

#' Extract specific event from BirdNET results file
#'
#' @description
#' Extract events detected by BirdNET-Analyzer from original recording files. As labels species names and a time-stamp are used. As guidance for manual inspection of results, a maximum of 10 samples per taxon are randomly selected for validation. Selected segments are highlighted using the keyword 'validate' in the Quality column of the resulting `BirdNET.xlsx` file. Furthermore, the Comments column is populated with a consecutive numbering (1:N) per taxon.
#'
#' @details
#' There are a couple of important prerequisites:
#'
#' (1) Run analyzer.py to detect events, making sure that --i and --o specify the same location.
#' (2) Reshape BirdNET results using function \code{\link{BirdNET}}
#'
#' @param path path. Relative path will be converted to absolute path.
#' @param output path to write wav files
#' @param taxon optional. select target taxa
#' @param score minimum confidence score
#' @param nmax maximum number of segments per taxon. defaults to NULL
#' @param sec seconds before and after target to extract
#' @param hyperlink optional. Insert hyperlink to audio file in xlsx document. Only if records are not filtered.
#' @param spectro logical. export spectro using \code{\link[bioacoustics]{spectro}} defaults to FALSE
#' @inheritParams BirdNET
#' @export
#'
BirdNET_extract <- function(path = NULL,
                            taxon = NULL,
                            score = NULL,
                            nmax = NULL,
                            sec = 1,
                            output = NULL,
                            #approx_snr = FALSE,
                            spectro = FALSE,
                            hyperlink = F,
                            model = c('BirdNET v2.4', 'Perch v2')) {

  ## binding for global variables to please checks ...
  name <- NA

  model <- match.arg(model)

  # Load existing workbook
  if (model ==        'BirdNET v2.4') {
    my_xlsx <-        'BirdNET.xlsx'
    my_pattern    <-  'BirdNET.results.txt'

  } else if (model == 'Perch v2') {
    my_xlsx <-        'Perch.xlsx'
    my_pattern <-     'Perch.results.txt'
  }

  if (!dir.exists(path)) stop("provide valid path")
  path <- tools::file_path_as_absolute(path)

  ## 1.) Check for BirdNET.xlsx and load if present
  if (!file.exists(file.path(path, my_xlsx))) stop(my_xlsx, " not found")
  xlsx <- readxl::read_xlsx(file.path(path, my_xlsx))
  #xlsx_head <- readxl::read_xlsx(path = db, sheet = 1, n_max = 1)

  ## check if BirdNET.xlsx was already formatted with BirdNET_extract
  ## Attempt by all files unique and containing extracted in file name
  if (!any(duplicated(xlsx$File)) & stringr::str_detect(xlsx$File[1], "extracted")) {
    stop(xlsx, " already formatted using BirdNET_extract!. Run again with BirdNET() will override existing data")
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
  if (isTRUE(spectro)) {
    cat("Extract events and create spectros ...\n")
  } else {
    cat("Extract events ...\n")
  }
  out <- pbapply::pbsapply(1:nrow(xlsx), function(r) {

    ## find wav file
    wav <- stringr::str_replace( xlsx[[r, "File"]], my_pattern, "WAV")
    format <- ".WAV"

    if (!file.exists(wav)) {
      ## check lower-case
      wav <- stringr::str_replace( xlsx[[r, "File"]], my_pattern, "wav")
      format <- ".wav"
    }

    if (!file.exists(wav)) stop(wav, "not found")

    ##  select event time stamps
    t0 <- xlsx[[r, "T0"]]; t1 <- xlsx[[r, "T1"]]; t2 <- xlsx[[r, "T2"]]

    ## create a suitable file name
    name <- file.path(
      output,
      xlsx[r, "Taxon"],
      paste0(
        xlsx[[r, "Taxon"]], "_",
        trimws(stringr::str_replace_all(
          as.character(t1), c(":" = "", "-" = "", " " = "_"))), format))


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

  ## put hyperlink(s) in excel sheet?
  if (isTRUE(hyperlink) & isFALSE(spectro)) {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, my_xlsx))

    ## create hyperlink ... check order?
    wav.link <- out; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)
    openxlsx::saveWorkbook(wb, file.path(path, my_xlsx), overwrite = TRUE)
  } else if (isTRUE(hyperlink) & isTRUE(spectro))  {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, my_xlsx))

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

    ## check for correct format pattern
    format <- tools::file_ext(out)

    png.link <- data.frame(
      png = file.path(file.path(dirname(out), "png"),
                      stringr::str_replace(string = basename(out),
                                           pattern = format,
                                           replacement = "png")))
    class(png.link$png) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = png.link,
                        startCol = 13,
                        colNames = TRUE,
                        startRow = 1)

    openxlsx::saveWorkbook(wb, file.path(path, my_xlsx), overwrite = TRUE)
  }

  ## Add information for validation of subsets

  for (one_taxon in unique(xlsx[["Taxon"]])) {
    ## find rows ...
    matching_rows <- which(xlsx[["Taxon"]] == one_taxon)
    ## set consecutive numbers ...
    xlsx[["Comment"]][matching_rows] <- 1:length(matching_rows)
    ## select segments for validation ...
    picked_samples <- sample_rows(x = matching_rows)
    # picked_samples <- ifelse(
    #   test = length(matching_rows) == 1,
    #   yes = matching_rows,
    #   no = sample(x =  as.numeric(matching_rows),
    #               size = ifelse(length(matching_rows) > 10, 10, length(matching_rows))))
    xlsx[["Quality"]][picked_samples] <- 'Validate !'
  }

  ## open workbook as it is ...
  wb <- openxlsx::loadWorkbook(file.path(path, my_xlsx))

  ## insert in wb ...
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x =  xlsx[["Comment"]],
                      startCol = 10,
                      colNames = FALSE,
                      startRow = 2)

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x =  xlsx[["Quality"]],
                      startCol = 9,
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::saveWorkbook(wb, file.path(path, my_xlsx), overwrite = TRUE)
  reformat_xlsx(path = path, model = model)

}
#' Visualise BirdNET results
#'
#' @param path path to BirdNET.xlsx file
#' @param taxa character
#' @import ggplot2 magrittr
#' @export
BirdNET_figure <- function(path, taxa) {

  ## binding for global variables to please checks ...
  mutate <- Taxon <- T1 <- CET <- Date <- Stunde <- sunrise <- sunset <- N <- NA
  ## retrieve lat, lon from Meta data if available, otherwise stick to defaults
  meta <- readxl::read_xlsx(path, "Meta")
  lat <- meta[["Lat"]]; lon <- meta[["Lon"]]

  ## create plot
  if (length(taxa) == 1) {
    ## read and format
    df <- readxl::read_xlsx(path, "Records") %>%
      dplyr::filter(Taxon %in% taxa) %>%
      dplyr::mutate(Date = lubridate::date(T1)) %>%
      dplyr::mutate(Stunde = lubridate::hour(T1)) %>%
      dplyr::group_by(Date, Stunde) %>%
      dplyr::summarise(N = length(Stunde))

    ## add sunrise and sunset
    df2 <- suncalc::getSunlightTimes(date = df$Date, lat = lat, lon = lon) %>%
      unique.data.frame() %>%
      dplyr::mutate(sunrise = lubridate::hour(sunrise) + lubridate::minute(sunrise)/60) %>%
      dplyr::mutate(sunset = lubridate::hour(sunset) + lubridate::minute(sunset)/60)
    df <- dplyr::left_join(df, df2, by = c("Date" = "date"))

    ggplot(df, aes(x = Stunde , y = Date, fill = N)) +
      geom_rect(aes(xmin = 0, xmax = sunrise, ymin = min(Date) - .5, ymax = max(Date) + .5),
                fill = "grey15", alpha = .01) +
      geom_rect(aes(xmin = sunset, xmax = 24, ymin = min(Date) - .5, ymax = max(Date) + .5),
                fill = "grey15", alpha = .01) +
      geom_tile(colour = "White") +
      scale_fill_binned(type = 'gradient') +
      geom_text(aes(label = N), col = "White") +
      scale_x_continuous(expand = c(0,0), limits = c(0,23), breaks = seq(0,23,3)) +
      scale_y_date(name = "", date_breaks = 'day', expand = c(0,0)) +
      egg::theme_article() +
      theme(panel.grid.minor.y = element_line(colour = "grey")) +
      labs(title = paste0(taxa, ': Nachweise pro Stunde'),x = "")
  } else {
    ## read and format
    df <- readxl::read_xlsx(path, "Records") %>%
      dplyr::filter(Taxon %in% taxa) %>%
      dplyr::mutate(Date = lubridate::date(T1)) %>%
      dplyr::mutate(Stunde = lubridate::hour(T1)) %>%
      dplyr::group_by(Taxon, Stunde) %>%
      dplyr::summarise(N = length(Stunde)) %>%
      cbind(Date = lubridate::date(meta[["From"]]))

    ## add sunrise and sunset
    df2 <- suncalc::getSunlightTimes(date = df$Date, lat = lat, lon = lon) %>%
      unique.data.frame() %>%
      dplyr::mutate(sunrise = lubridate::hour(sunrise) + lubridate::minute(sunrise)/60) %>%
      dplyr::mutate(sunset = lubridate::hour(sunset) + lubridate::minute(sunset)/60)
    df <- dplyr::left_join(df, df2, by = c("Date" = "date"))
    ## sort taxa
    df$Taxon <- factor(df$Taxon, levels = sort(df$Taxon, decreasing = T) %>% unique())
    ggplot(df, aes(x = Stunde , y = Taxon, fill = N)) +

      geom_tile(colour = "White") +
      scale_fill_binned(type = 'gradient') +
      geom_text(aes(label = N), col = "White") +
      scale_x_continuous(expand = c(0,0), limits = c(0,23), breaks = seq(0,23,3)) +
      egg::theme_article() +
      theme(panel.grid.minor.y = element_line(colour = "grey")) +
      labs(x = "", caption = paste0(meta$From, " - ", meta$To), y = "")
  }

}

#' Hide wav files from BirdNET analyze.py
#'
#' @description
#' Temporarily move all wav file that where already processed, i.e. `BirdNET.results.txt` exists to another location to avoid rerunning analysis by `BirdNET_analyzer`.
#' Ideally, this is coded directly in analyze.py or its auxiliary functions defined in utils.py
#'
#' @param path path
#' @param hidden.path path
#'
#' @export
#'
BirdNET_hide <- function(path, hidden.path) {

  ## binding for global variables to please checks ...
  # end <- sec <- start <- sound.files <- . <- NA

  ## list BirdNET.results.txt files
  BirdNET.results <- list.files(path, recursive = T, pattern = "BirdNET.results.txt", full.names = F)

  ## list audio files
  audio <- list.files(path, recursive = T, pattern = ".WAV", ignore.case = T, full.names = F)

  ## check for corresponding BirdNET.results.txt
  audio <- unlist(lapply(audio, function(x) {
    y <- stringr::str_replace(string = x, pattern = tools::file_ext(x), replacement = "BirdNET.results.txt")
    if (y %in% BirdNET.results) {
      return(x)
    } else {
      return(character())
    }
  }))

  if (length(audio > 0)) {
    ## move files and keep a record
    cat("Found", length(audio), " files that were already analysed\n")
    df <-  data.frame(from = file.path(path, audio),
                      to =  file.path(hidden.path, basename(audio)))
    utils::write.table(df, file = file.path(hidden.path, paste0(basename(path), ".txt")))
    silent <- file.rename(from = df$from, to = df$to)
  }
}


#' Unhide wav files from BirdNET analyze.py
#' @inheritParams BirdNET_hide
#' @export
#'
BirdNET_unhide <- function(path, hidden.path) {
  df <- utils::read.table(file = file.path(hidden.path, paste0(basename(path), ".txt")))
  silent <- file.rename(from = df$to, to = df$from)
}

#' Append manually recovered detections to existing xlsx file
#'
#' @details
#' Reads audacity labels from a text file and adds to BirdNET.xlsx
#'
#'
#' @inheritParams BirdNET
#' @inheritParams BirdNET_extract
#' @param recursive logical
#' @export
#'
BirdNET_man_detec <- function(path, spectro = FALSE, recursive = FALSE) {


  ## check for manual detections
  Records <- BirdNET_labels2results(path = path, recursive = recursive)

  if (nrow(Records) >= 1) {
    openxlsx::write.xlsx(x = Records, file = file.path(path, "BirdNET2.xlsx"), overwrite = T)
    if (isTRUE(spectro)) warning("Due to an internal bug Spectro = TRUE is currently not supported!")
    #BirdNET_extract2(path = path, spectro = spectro)
    BirdNET_extract2(path = path, spectro = FALSE)

    ## now append to BirdNET.xlsx and delete BirdNET2.xlsx
    ## read db if existing ...

    ## read db to detect duplicates before writing ...
    ## openxlsx does not handle format correctly ...
    db1 <- readxl::read_xlsx(path = file.path(path, "BirdNET.xlsx"), sheet = 1)
    ## drop empty rows
    db1 <- db1[rowSums(is.na(db1)) != ncol(db1),]
    db1_rows <- nrow(db1)

    db2 <- readxl::read_xlsx(path = file.path(path, "BirdNET2.xlsx"), sheet = 1)
    ## drop empty rows
    db2 <- db2[rowSums(is.na(db2)) != ncol(db2),]

    if ("png" %in% names(db1) & !"png" %in% names(db2)) {
      db2[["png"]] <- NA
    }

    if (any(duplicated(db1))) warning("Database contains duplicates!")
    db_new <- try(rbind(db1, db2))
    if (!methods::is(db_new, "try-error")) {
      db_new <- dplyr::bind_rows(db1, db2)
    }


    if (any(duplicated(db_new))) {
      stop("Database already contains entries. Skip")
    }

    ## append db2 to db1
    db1 <- file.path(path, "BirdNET.xlsx")

    ## create hyperlink ... check order?
    class(db2$File) <- "hyperlink"

    if ("png" %in% names(db2)) class(db2$png) <- "hyperlink"

    wb <- openxlsx::loadWorkbook(db1)
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = db2,
                        colNames = FALSE,
                        startRow = db1_rows + 2)
    openxlsx::saveWorkbook(wb, file = db1, overwrite = TRUE)
    unlink(file.path(path, "BirdNET2.xlsx"))
  }

}

#' Write metadata of a recording project
#'
#' @description
#' Add metadata to recording summary. Including
#' (1) Recording location and parameters
#' (2) BirdNET analysis settings
#'
#' @param Lat latitude
#' @param Lon longitude
#' @param Location location name
#' @param Device recording device
#' @param Micro external microphone. default 'none'
#' @param Min_conf min_conf used in BirdNET-Analyzer
#' @param Overlap overlap used in BirdNET-Analyzer
#' @param Sensitivity sensitivity used in BirdNET-Analyzer
#' @param Slist path to slist used in BirdNET-Analyzer. default full model \code{'BirdNET_V2.4'}
#' @return data frame
#' @export
#'
BirdNET_meta <- function(
    Location = NA,
    Lat = NA,
    Lon = NA,
    Device = NA,
    Micro = NA,
    Min_conf = 0.7,
    Overlap = 0,
    Sensitivity = 1.25,
    Slist = "BirdNET_V2.4") {

  # Device <- match.arg(Device)
  # Micro <- match.arg(Micro)

  data.frame(Location,
             Lat,
             Lon,
             From = NA,
             To = NA,
             Duration = NA,
             Device,
             Micro,
             Min_conf,
             Overlap,
             Sensitivity,
             Slist)

}
#' Modify 'BirdNET.results.txt' created by BirdNET-Analyzer
#'
#' @param path path to text files
#' @param recursive logical. Should the listing recurse into directories?
#' @inheritParams BirdNET
#' @description
#' Tweaks audacity labels by composing a string consisting of species name, recording date and time and detection score (e.g. "Blackbird 2023-08-15 [0.89]").
#'
#' @details
#' #' Reads text file containing audacity labels that were created by running the BirdNET analyzer script. Note, specifying rtype 'audacity' is required. Output is reformatted by adding the correct time stamp (estimated from file names) to the detected events. Empty files are ignored
#'
#' @return data frame
#'
#' @keywords internal
#'
BirdNET_results2txt <- function(path = NULL, recursive = FALSE, model = c('BirdNET v2.4', 'Perch v2')) {

  if (!dir.exists(path)) stop("provide valid path")
  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    my_pattern    <- 'BirdNET.results.txt'
    my_replacement <- "BirdNET.labels.txt"
  } else if (model == 'Perch v2') {
    my_pattern <- 'Perch.results.txt'
    my_replacement <- 'Perch.labels.txt'
  }

  ## Check 'BirdNET.results.txt'
  BirdNET.results.files <- list.files(path = path,
                                      pattern = my_pattern,
                                      full.names = T,
                                      recursive = recursive)

  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))

  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }


  if (length(BirdNET.results.files) >= 1) {
    BirdNET.results.list <- lapply(BirdNET.results.files, read.audacity)
    results <- data.frame()

    for (i in 1:length(BirdNET.results.files)) {
      BirdNET.results.df <- BirdNET.results.list[[i]]
      BirdNET.results.df$Score <- round(BirdNET.results.df$Score, 3)
      ## prepare head
      BirdNET.results.df$x <- sapply(BirdNET.results.df$file, function(x) {
        out <- stringr::str_split(x, "/")[[1]]
        as.character(out[length(out)])
      })

      ## Get recording time from head
      BirdNET.results.df$Start <- RecreateDateTime(BirdNET.results.df$x)

      ## extract common name form label
      BirdNET.results.df$label2 <- sapply(BirdNET.results.df$label, function(x) {
        t <- stringr::str_split(x, ", ")[[1]]
        return(t[length(t)])
      })

      ## check for special case: 00:00:00 & t1 = 0
      if (any(BirdNET.results.df$t1 == 0)) {
        indices <- which(BirdNET.results.df$t1 == 0)
        for (ii in indices) {
          time.sum <-
            lubridate::hour(BirdNET.results.df$Start[ii]) +
            lubridate::minute(BirdNET.results.df$Start[ii]) +
            lubridate::second(BirdNET.results.df$Start[ii])
          if (time.sum == 0) BirdNET.results.df$t1[ii] <- 1
        }
      }

      BirdNET.results.df$labelNEW <- paste(
        BirdNET.results.df$label2,
        BirdNET.results.df[["Start"]] + BirdNET.results.df[["t1"]],
        "[", BirdNET.results.df[["Score"]],"]")

      ## write to file
      seewave::write.audacity(
        x = data.frame(
          label = BirdNET.results.df[["labelNEW"]],
          t1 = BirdNET.results.df[["t1"]],
          t2 = BirdNET.results.df[["t2"]]),
        filename = stringr::str_replace(string = BirdNET.results.df$file[1],
                                        pattern = my_pattern ,
                                        replacement = my_replacement))
      results <- rbind(results, BirdNET.results.df)
    }


  } else {
    stop("Did not find any ", my_pattern,  " files!")
  }
  return(results)
}

#' Create custom species list by picking species from `BirdNET_GLOBAL_6K_V2.4_Labels`
#'
#' @description
#' Loads \strong{BirdNET_GLOBAL_6K_V2.4_Labels} from [BirdNET-Analyzer](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4) and filters for target species.
#'
#' @details
#' Requires that BirdNET_GLOBAL_6K_V2.4_Labels are locally available. The easiest way is to simply clone the entire [BirdNET-Analyzer] (https://github.com/kahst/BirdNET-Analyzer) repository. Alternatively manually download the needed folders [checkpoints/V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/checkpoints/V2.4) and [labels/V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4) and save them within one directory, maintaining the original subfolders.
#'
#' @param names species names as character vector. Either scientific name or common names according to specific \code{lang}.
#' @param sciNames logical. default \code{FALSE}
#' @param lang Two-letter language code denoting a label file available for [BirdNET_Global_6K_V2.4](https://github.com/kahst/BirdNET-Analyzer/tree/main/labels/V2.4)
#' @param BirdNET_path path to BirdNET-Analyzer.
#' @param species_list path for exporting the species list
#' @param .write_text logical. defaults to \code{TRUE}
#' @return data frame
#' @examples
#' \dontrun{BirdNET_species.list(names = c("Accipiter nisus"), sciNames = T, .write_text = F)}
#'
#' @export
#'
BirdNET_species.list <- function(
    names = NULL,
    sciNames = FALSE,
    lang = 'de',
    BirdNET_path = "../BirdNET-Analyzer/",
    species_list = "my_custom_species_list.txt",
    .write_text = TRUE) {

  ## check function call
  if (is.null(names)) stop("Specfiy a character vector of target species")
  if (nchar(lang) > 2) {
    stop("Specify a two-letter langugage code: ", lang, " does not work")
  } else {
    ## force to lower-case
    lang <- tolower(lang)

    ## try to find corresponding labels
    path2labels_en <- file.path(
      BirdNET_path, 'checkpoints/V2.4',
      paste0('BirdNET_GLOBAL_6K_V2.4_Labels', ".txt"))

    if (!file.exists(path2labels_en)) {
      stop("Trying to access ", path2labels_en, " failed!")
    } else {
      labels_en <- utils::read.delim(path2labels_en, sep = "_", col.names = c("sciName", "comName"))
    }

    if (lang != "en") {
      path2labels_xy <- file.path(
        BirdNET_path, 'labels/V2.4',
        paste0('BirdNET_GLOBAL_6K_V2.4_Labels_', lang, ".txt"))

      if (!file.exists(path2labels_xy)) {
        stop("Trying to access ", path2labels_xy, " failed!")
      }  else {
        labels_xy <- utils::read.delim(path2labels_xy, sep = "_", col.names = c("sciName", "comNameXY"))
      }

      labels <- dplyr::left_join(labels_en, labels_xy, by = "sciName")

    } else {
      labels <- labels_en
      names(labels)[2] <- "comNameXY"
    }

    ## subset according to name vector

    if (isFALSE(sciNames)) {
      ## subset species list
      comNameXY <- NA
      df <- dplyr::filter(labels, comNameXY %in% names)
    } else {
      sciName <- NA
      df <- dplyr::filter(labels, sciName %in% names)
    }
    df <- df[,c("sciName", "comName")]

    ## write to file
    if (isTRUE(.write_text)) utils::write.table(x = df, sep = "_", col.names = FALSE, row.names = FALSE, file = species_list, quote = FALSE)
    return(df)
  }
}
#' Performs a cleanup of sound archives
#'
#' @details
#' This function is called once files were analysed using BirdNET-Analyzer. Then empty BirdNET.results files are erased as well as wave files containing no audio
#'
#'
#' @inheritParams BirdNET_results2txt
#' @export
#'
BirdNET_tidyup <- function(path, recursive, model = c('BirdNET v2.4', 'Perch v2')) {


  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    my_pattern    <- 'BirdNET.results.txt'
  } else if (model == 'Perch v2') {
    my_pattern <- 'Perch.results.txt'
  }

  ## find results files
  BirdNET.results.files <- list.files(path = path,
                                      pattern = my_pattern,
                                      full.names = T,
                                      recursive = recursive)
  ## identify empty files ...
  empty_files <- as.logical((sapply(BirdNET.results.files, file.size) == 0))
  ## erase ...
  if (any(empty_files == TRUE)) {
    unlink(BirdNET.results.files[empty_files == TRUE])
    BirdNET.results.files <- BirdNET.results.files[which(empty_files == FALSE)]
  }

  # ## identify empty recordings ...
  wavs <- list.files(path = path, pattern = ".WAV",
                     ignore.case = T, recursive = recursive, full.names = T)
  wavs.size <- file.size(wavs)

  if (any(wavs.size == 0)) unlink(wavs[wavs.size == 0])
}

#' Cleanup taxon names to avoid problems when specifying path
#'
#' @description
#' Fix the slash '/' in the taxon name and replace by a minus '-'-
#'
#' @param path path
#' @inheritParams BirdNET
#' @keywords internal
#' @import openxlsx
#' @import readxl
#'
BirdNET_name_repair <- function(path, model = c('BirdNET v2.4', 'Perch v2')) {

  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    xlsx    <- 'BirdNET.xlsx'
  } else if (model == 'Perch v2') {
    xlsx <- 'Perch.xlsx'
  }

  xlsx_path <- file.path(path, xlsx)
  wb <- openxlsx::loadWorkbook(xlsx_path)
  df <- readxl::read_xlsx(xlsx_path)

  Taxon <- NA

  check <- dplyr::filter(df, grepl("/", Taxon))

  if (nrow(check) > 1) {
    df[['Taxon']] <- gsub("/", "-", df[['Taxon']], fixed = TRUE)
    openxlsx::writeData(wb,
                        sheet = 1,
                        x = df$Taxon,
                        startCol = which(names(df) == "Taxon"),
                        startRow = 2,
                        colNames = FALSE)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    return(data.frame(old = check[["Taxon"]],
                      new = gsub("/", "-", check[['Taxon']], fixed = TRUE)))
  } else {
    return(data.frame())
  }
}

#' Data frame of BirdNET detections
#'
#' @param path path
#'
#' @import magrittr
#' @inheritParams BirdNET_results2txt
#' @keywords internal
#'
BirdNET_table <- function(path = NULL, recursive = FALSE, model = c('BirdNET v2.4', 'Perch v2')) {
  if (!dir.exists(path)) stop("provide valid path")

  ## binding for global variables to please checks ...
  label <- time <- species <- hour <- . <- Taxon <- NA

  model <- match.arg(model)

  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    my_pattern <- "BirdNET.labels.txt"
  } else if (model == 'Perch v2') {
    my_pattern <- 'Perch.labels.txt'
  }

  # 0.) Check 'BirdNET.results.txt'
  BirdNET.results.files <- list.files(path = path,
                                      pattern = my_pattern,
                                      full.names = T,
                                      recursive = recursive)


  if (length(BirdNET.results.files) >= 1) {
    # read audacity marks
    if (interactive()) {
      message("Read ", model,  " results:\n")
      df <- pbapply::pblapply(BirdNET.results.files,
                              readr::read_delim,
                              delim = "\t",
                              progress = FALSE,
                              show_col_types = FALSE,
                              col_names = c("start", "end", "label")) %>%
        do.call("rbind",.)
    } else {
      df <- lapply(BirdNET.results.files,
                   readr::read_delim,
                   delim = "\t",
                   progress = FALSE,
                   show_col_types = FALSE,
                   col_names = c("start", "end", "label")) %>%
        do.call("rbind",.)
    }

    ## split label into single variables
    df <- df %>%
      mutate(
        species = stringr::str_extract(label, "^.*(?=\\s\\d{4}-\\d{2}-\\d{2})"),
        date    = as.Date(stringr::str_extract(label, "\\d{4}-\\d{2}-\\d{2}")),
        time    = as.POSIXct(stringr::str_extract(label, "\\d{2}:\\d{2}:\\d{2}"),
                             format = "%H:%M:%S"),
        score   = as.numeric(stringr::str_extract(label, "\\d+\\.\\d+(?=\\s*\\])")),
        hour = lubridate::hour(time))

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


