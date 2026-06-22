#' Extract events detected by [chirpity](https://chirpity.mattkirkland.co.uk/) and append to `BirdNET.xlsx`
#'
#' @description
#' see \link{BirdNET_extract}
#'
#' @details
#' extract events exclusively detected by [chirpity](https://chirpity.mattkirkland.co.uk/) and add to `BirdNET.xlsx`
#'
#' @inheritParams BirdNET_extract
#' @param input xlsx file containing BirdNET and chirpity results
#' @import magrittr
#' @export
#'
chirpity_extract <- function(path = NULL,
                             input = file.path(path, "BirdNET_Chirpity.xlsx"),
                             output = NULL,
                             spectro = FALSE,
                             hyperlink = FALSE,
                             sec = 1) {

  .Deprecated(msg = 'This function is currently not further maintained!')

  ## no visible binding for global variable ------------------
  Detector <- . <- NULL

  if (!dir.exists(path)) stop("provide valid path")
  path <- tools::file_path_as_absolute(path)

  ## Read BirdNET.xlsx ---------------------------------------
  if (!file.exists(input)) stop("chirpity not found")
  detection_table <- readxl::read_xlsx(input) %>%
    ## add counter ...
    dplyr::mutate(counter = 1:nrow(.))

  xlsx <- detection_table %>%
    ## select Detector == 'chirpity'
    dplyr::filter(., Detector == 'chirpity')

  ## Create output folder ------------------------------------
  if (is.null(output)) output <- file.path(path, "extracted")
  if (!dir.exists(output)) dir.create(output, showWarnings = FALSE)

  silent <- sapply(unique(xlsx[["Taxon"]]), function(one_taxon) {
    if (!dir.exists(file.path(output, one_taxon))) {
      dir.create(file.path(output, one_taxon), showWarnings = FALSE)
    }})

  ## Extract audio -------------------------------------------
  if (isTRUE(spectro)) {
    cat("Extract events and create spectros ...\n")
  } else {
    cat("Extract events ...\n")
  }

  out <- pbapply::pbsapply(1:nrow(xlsx), function(r) {

    ### check wav file ---------------------------------------
    if (!file.exists(xlsx[["File"]][r])) stop(xlsx[["File"]][r], "not found")

    ##  select event time stamps -----------------------------
    t0 <- xlsx[[r, "T0"]]; t1 <- xlsx[[r, "T1"]]; t2 <- xlsx[[r, "T2"]]

    ## create a suitable file name
    name <- file.path(
      output,
      xlsx[r, "Taxon"],
      paste0(
        xlsx[[r, "Taxon"]], "_",
        trimws(stringr::str_replace_all(
          as.character(t1), c(":" = "", "-" = "", " " = "_"))), '.', tools::file_ext(xlsx[["File"]][r])))

    ## add buffer around event -------------------------------
    from = as.numeric(difftime(t1, t0, units = "secs")) - sec
    if (from < 0) from <- 0
    to = as.numeric(difftime(t2, t0, units = "secs")) + sec
    if (to > t2) to <- t2

    ## extract event -----------------------------------------
    event <- tuneR::readWave(
      filename = xlsx[["File"]][r],
      from = from,
      to = to,
      units = "seconds")

    ## export audio segments ---------------------------------
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
                  stringr::str_replace(basename(name), paste0('.', tools::file_ext(xlsx[["File"]][r])), ".png")),
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

  ## put new file names in detection_table
  detection_table[["File"]][xlsx[["counter"]]] <-
    out

  ## Hyperlinks ----------------------------------------------
  if (isTRUE(hyperlink) & isFALSE(spectro)) {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(input)

    ## create hyperlink ... check order?
    wav.link <- detection_table[["File"]]; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)
    openxlsx::saveWorkbook(wb, input, overwrite = TRUE)
  } else if (isTRUE(hyperlink) & isTRUE(spectro))  {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(input)

    ## create hyperlink ... check order?
    wav.link <- detection_table[["File"]]; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)

    ## check for correct format pattern
    format <- tools::file_ext(detection_table[["File"]])

    png.link <- data.frame(
      png = file.path(file.path(dirname(detection_table[["File"]]), "png"),
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

    openxlsx::saveWorkbook(wb, input, overwrite = TRUE)
  }

  ## Add information for validation of subsets

  for (one_taxon in unique(xlsx[["Taxon"]])) {
    ## find rows ...
    matching_rows <- which(xlsx[["Taxon"]] == one_taxon)
    ## set consecutive numbers ...
    xlsx[["Comment"]][matching_rows] <- 1:length(matching_rows)
    ## select segments for validation ...
    picked_samples <- sample_rows(x = matching_rows)
    xlsx[["Quality"]][picked_samples] <- 'Validate !'
  }

  ## open workbook as it is ...
  wb <- openxlsx::loadWorkbook(input)

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
  openxlsx::saveWorkbook(wb, input, overwrite = TRUE)
}

#' Combine event detections of [BirdNET](github.com/kahst/BirdNET-Analyzer) and [chirpity](https://chirpity.mattkirkland.co.uk/)
#'
#' @description
#' Merges detection tables created by [BirdNET](github.com/kahst/BirdNET-Analyzer) and [chirpity](https://chirpity.mattkirkland.co.uk/) models. Events are classified as identical when Taxon and Event timestamp T1 are the same. Because [chirpity](https://chirpity.mattkirkland.co.uk/) further differentiates certain call types, extensions such as 'call' or 'flight call' are ignored when specified by param ignore_terms
#'
#' @param path path. Relative path will be converted to absolute path.
#' @param birdnet path to BirdNET.xlsx file
#' @param chirpity path to chirpity.csv file
#' @param output defaults to file.path(path, "BirdNET_Chirpity.xlsx")
#' @param ignore_terms character vector of text to remove when in parentheses (e.g. '(call)' or '(flight call)')
#'
#' @import magrittr
#' @importFrom utils read.csv
#' @export
#'
chirpity2BirdNET <- function(
    path = NULL,
    birdnet = file.path(path, "BirdNET.xlsx"),
    chirpity = file.path(path, "Chirpity.csv"),
    output = file.path(path, "BirdNET_Chirpity.xlsx"),
    ignore_terms = c('call', 'flight call', 'song')) {

  .Deprecated(msg = 'This function is currently not further maintained!')

  ## no visible binding for global variable ------------------
  Common.name <- Model <- File <- Start..s. <- Confidence <-
    . <- png <- Taxon <- NULL

  ## read birdnet --------------------------------------------
  birdnet_output <- readxl::read_xlsx(birdnet)
  birdnet_meta <- readxl::read_xlsx(birdnet, sheet = "Meta")

  ## read chirpity -------------------------------------------
  chirpity_output <- utils::read.csv(chirpity) %>%
    ### format according to birdnet template -----------------
  dplyr::mutate(Taxon = Common.name,
                Detector = Model,
                ID = NA,
                T1 = get_timestamp(File) + `Start..s.`,
                T2 = T1 + 3,
                Score = Confidence,
                Verification = NA,
                Correction = NA,
                Quality = NA,
                Comment = NA,
                T0 = get_timestamp(File),
                File = gsub("\\\\", "/", File)) %>%
    subset(., select = names(birdnet_output))

  if ('png' %in% names(chirpity_output)) {
    chirpity_output <- chirpity_output %>%
      dplyr::mutate(png = gsub("\\\\", "/", png))
  }

  ### adjust taxon names -------------------------------------
  if (!is.null(ignore_terms)) {
    ## create regex pattern
    ignore_terms <- paste0("\\(", ignore_terms, "\\)")
    ## remove pattern
    chirpity_output[["Taxon"]] <- stringr::str_remove_all(chirpity_output[["Taxon"]], pattern = paste(ignore_terms, collapse = "|"))
    ## trim whitespaces
    chirpity_output[["Taxon"]] <- trimws(chirpity_output[["Taxon"]])
  }

  ## merge tables --------------------------------------------
  merged <- rbind(birdnet_output, chirpity_output)

  ## merge duplicates ----------------------------------------
  T1 <- as.character(unique(merged[["T1"]]))

  merged <- lapply(T1, function(t) {
    df <- dplyr::filter(merged, as.character(T1) == t)

    ## if T1 and Taxon are identical
    if (nrow(df) == 2 & length(unique(df[["Taxon"]])) == 1) {
      df[["Detector"]][1] <- paste0(df[["Detector"]][1], ';', df[["Detector"]][2])
      df[["Score"]][1] <- mean(df[["Score"]])
      return(df[1,])
      ## all other cases
    } else {
      return(df)
    }
  }) %>%
    do.call("rbind",.)

  ## sort rows -----------------------------------------------
  # Sort by name (A-Z), then age (ascending)
  # merged <- merged %>%
  #   dplyr::arrange(Taxon, T1)

  ## export to xlsx ------------------------------------------

  out <- list(Records = merged, Meta = birdnet_meta)
  openxlsx::write.xlsx(x = out, file = output, overwrite = T)

  ## create hyper links --------------------------------------
  if (!'png' %in% names(birdnet_output)) { ## no spectro

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(output)

    ## create wave hyperlink
    wav.link <- merged[["File"]]; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)
    openxlsx::saveWorkbook(wb, output, overwrite = TRUE)
  } else if ('png' %in% names(birdnet_output))  {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(output)

    ## create wave hyperlink
    wav.link <- merged[["File"]]; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)

    ## create hyperlink ... check order?

    ## check for correct format pattern
    format <- tools::file_ext(merged[["File"]])

    png.link <- data.frame(
      png = file.path(file.path(dirname(merged[["File"]]), "png"),
                      stringr::str_replace(string = basename(merged[["File"]]),
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

    openxlsx::saveWorkbook(wb,output, overwrite = TRUE)
  }

  ## Add information for validation of subsets

  for (one_taxon in unique(merged[["Taxon"]])) {
    ## find rows ...
    matching_rows <- which(merged[["Taxon"]] == one_taxon)
    ## set consecutive numbers ...
    merged[["Comment"]][matching_rows] <- 1:length(matching_rows)
    ## select segments for validation ...
    picked_samples <- sample_rows(x = matching_rows)
    # picked_samples <- ifelse(
    #   test = length(matching_rows) == 1,
    #   yes = matching_rows,
    #   no = sample(x =  as.numeric(matching_rows),
    #               size = ifelse(length(matching_rows) > 10, 10, length(matching_rows))))
    merged[["Quality"]][picked_samples] <- 'Validate !'
  }

  ## open workbook as it is ...
  wb <- openxlsx::loadWorkbook(output)

  ## insert in wb ...
  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x =  merged[["Comment"]],
                      startCol = 10,
                      colNames = FALSE,
                      startRow = 2)

  openxlsx::writeData(wb = wb,
                      sheet = 1,
                      x =  merged[["Quality"]],
                      startCol = 9,
                      colNames = FALSE,
                      startRow = 2)
  openxlsx::saveWorkbook(wb, output, overwrite = TRUE)

  return(merged)

  # ## audacity labels
  # wave_files <- unique(merged[["File"]])
  #
  #
  # out <- lapply(wave_files, function(wave) {
  #   df <- dplyr::filter(chirpity, File == wave)
  #   ## prepare audacity labels
  #   seewave::write.audacity(
  #     x = data.frame(
  #       label = paste(df[["Common.name"]], df[["EventTime"]], paste0('[', df[["Confidence"]], ']')),
  #       t1 = df[["Start..s."]],
  #       t2 = df[["Start..s."]] + 3),
  #     filename = stringr::str_replace(wave, tools::file_ext(wave), paste0(model,'.chirpity.txt')))
  # })

}


#' Convert chirpity results to audacity labels
#'
#' @param path path to chirpity results file (.csv)
#' @import magrittr
#' @importFrom seewave write.audacity
#' @importFrom utils read.csv
#' @export
#'
chirpity2audacity <- function(path) {

  .Deprecated(msg = 'This function is currently not further maintained!')

  ## no visible binding for global variable ------------------
  File <- Time <- Start..s. <- NULL

  ## read csv file ...
  chirpity <- utils::read.csv(path) %>%
    dplyr::mutate(
      # reformat file path
      File = gsub("\\\\", "/", File),
      ## correct time stamp
      Time = get_timestamp(File),
      EventTime = Time + `Start..s.`)

  ## retrieve model
  model <- unique(chirpity[["Model"]])

  ## split by file
  wave_files <- unique(chirpity[["File"]])

  out <- lapply(wave_files, function(wave) {
    df <- dplyr::filter(chirpity, File == wave)
    ## prepare audacity labels
    seewave::write.audacity(
      x = data.frame(
        label = paste(df[["Common.name"]], df[["EventTime"]], paste0('[', df[["Confidence"]], ']')),
        t1 = df[["Start..s."]],
        t2 = df[["Start..s."]] + 3),
      filename = stringr::str_replace(wave, tools::file_ext(wave), paste0(model,'.chirpity.txt')))
  })

}

