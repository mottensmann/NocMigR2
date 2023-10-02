#' Extract specific event from BirdNET results file
#'
#' @details
#' Prerequisites:
#' (1) Run analyzer.py (same folder specified for --i and --o!)
#' (2) Reshape output using function BirdNET (this package)
#' (3) Ensure wav files, BirdNET_results.txt and BirdNET.xlsx files share the same path
#'
#' @param path path. Relative path will be converted to absolute path.
#' @param output path to write wav files
#' @param taxon optional. select target taxa
#' @param score minimum confidence score
#' @param nmax maximum number of segments per taxon. default 10
#' @param sec seconds before and after target to extract
#' @param hyperlink optional. Insert hyperlink to audio file in xlsx document. Only if records are not filtered.
#' @param approx_snr logical. trying to approximate SNR levels. defaults to FALSE
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
                            approx_snr = FALSE,
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
  out <- pbapply::pbsapply(1:nrow(xlsx), function(r) {

    ## find wav file
    ##  ------------------------------------------------------------------------
    wav <- stringr::str_replace( xlsx[[r, "File"]], "BirdNET.results.txt", "WAV")
    format <- ".WAV"

    if (!file.exists(wav)) {
      ## check lowercase
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

    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
  }

  if (isTRUE(approx_snr)) {

    cat("Compute SNRs ... \n")
    SNR <- BirdNET_snr(path = path)
    cat("done\n")
    SNR <- dplyr::left_join(data.frame(sound.files = basename(out)),
                            SNR, by = "sound.files")
    SNR <- data.frame(SNR = floor(SNR$SNR))
    ## open workbook as it is ...
    wb <- openxlsx::loadWorkbook(file.path(path, "BirdNET.xlsx"))

    ## insert in wb ...
    openxlsx::writeData(wb = wb,
                        sheet = 1,
                        x = SNR,
                        startCol = ncol(openxlsx::readWorkbook(file.path(path, "BirdNET.xlsx"))) + 1,
                        colNames = TRUE,
                        startRow = 1)
    openxlsx::saveWorkbook(wb, file.path(path, "BirdNET.xlsx"), overwrite = TRUE)
  }
}
