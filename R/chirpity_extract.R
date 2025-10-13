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

