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



