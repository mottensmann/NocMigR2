#' Convert chirpity results to audacity labels
#'
#' @param path path to chirpity results file (.csv)
#' @import magrittr
#' @importFrom seewave write.audacity
#' @importFrom utils read.csv
#' @export
#'
chirpity2audacity <- function(path) {

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
