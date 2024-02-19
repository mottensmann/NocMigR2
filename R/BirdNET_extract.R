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
#'
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
                            hyperlink = F) {

  ## binding for global variables to please checks ...
  name <- NA

  if (!dir.exists(path)) stop("provide valid path")
  path <- tools::file_path_as_absolute(path)

  ## 1.) Check for BirdNET.xlsx and load if present
  ## ---------------------------------------------------------------------------
  if (!file.exists(file.path(path, "BirdNET.xlsx"))) stop("BirdNET.xlsx not found")
  xlsx <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx"))

  ## check if BirdNET.xlsx was already formatted with BirdNET_extract
  ## Attempt by all files unique and containing extracted in file name
  if (!any(duplicated(xlsx$File)) & stringr::str_detect(xlsx$File[1], "extracted")) {
    stop("BirdNET.xlsx already formatted using BirdNET_extract!. Run again with BirdNET() will override existing data")
  }

  ## 2.) Optional: Subset results to extract
  ## ---------------------------------------------------------------------------
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
  ## -------------------------------------------------------------------------
  if (is.null(output)) output <- file.path(path, "extracted")
  if (!dir.exists(output)) dir.create(output, showWarnings = FALSE)

  silent <- sapply(unique(xlsx[["Taxon"]]), function(one_taxon) {
    if (!dir.exists(file.path(output, one_taxon))) {
      dir.create(file.path(output, one_taxon), showWarnings = FALSE)
    }})

  ## 3.) Extract ...
  ## ---------------------------------------------------------------------------
  if (isTRUE(spectro)) {
    cat("Extract events and create spectros ...\n")
  } else {
    cat("Extract events ...\n")
  }
  out <- pbapply::pbsapply(1:nrow(xlsx), function(r) {

    ## find wav file
    ##  ------------------------------------------------------------------------
    wav <- stringr::str_replace( xlsx[[r, "File"]], "BirdNET.results.txt", "WAV")
    format <- ".WAV"

    if (!file.exists(wav)) {
      ## check lower-case
      wav <- stringr::str_replace( xlsx[[r, "File"]], "BirdNET.results.txt", "wav")
      format <- ".wav"
    }

    if (!file.exists(wav)) stop(wav, "not found")

    ##  select event time stamps
    ##  ------------------------------------------------------------------------
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
    ## -------------------------------------------------------------------------
    from = as.numeric(difftime(t1, t0, units = "secs")) - sec
    if (from < 0) from <- 0
    to = as.numeric(difftime(t2, t0, units = "secs")) + sec
    if (to > t2) to <- t2

    ## extract event
    ##  ------------------------------------------------------------------------
    event <- tuneR::readWave(
      filename = wav,
      from = from,
      to = to,
      units = "seconds")

    ## export audio segments
    ## -------------------------------------------------------------------------
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
  ## -------------------------------------------------------------------------
  if (isTRUE(hyperlink) & isFALSE(spectro)) {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))

    ## create hyperlink ... check order?
    wav.link <- out; class(wav.link) <- "hyperlink"

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = wav.link,
                        startCol = 12,
                        colNames = FALSE,
                        startRow = 2)
    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
  } else if (isTRUE(hyperlink) & isTRUE(spectro))  {

    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))

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

    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
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
  wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))

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
  openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)


  # Sat Nov 18 11:25:01 2023 ------------------------------
  # SNR does not work this way. check later again

  # if (isTRUE(approx_snr)) {
  #
  #   cat("Compute SNRs ... \n")
  #   SNR <- BirdNET_snr(path = path)
  #   cat("done\n")
  #   SNR <- dplyr::left_join(data.frame(sound.files = basename(out)),
  #                           SNR, by = "sound.files")
  #   SNR <- data.frame(SNR = floor(SNR$SNR))
  #   ## open workbook as it is ...
  #   wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))
  #
  #   ## insert in wb ...
  #   openxlsx::writeData(wb = wb,
  #                       sheet = 1,
  #                       x = SNR,
  #                       startCol = ncol(openxlsx::readWorkbook(file.path(path, "BirdNET.xlsx"))) + 1,
  #                       colNames = TRUE,
  #                       startRow = 1)
  #   openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
  # }
}
