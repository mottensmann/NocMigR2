#' Compute Audio Moth recording days
#'
#' @description
#' Approximates the possible recording duration in days, taking energy consumption and file size into account. Estimated file size and energy usage can be calculated using the `AudioMoth Configuration App` (see reference below)
#' @references https://github.com/OpenAcousticDevices/AudioMoth-Configuration-App
#'
#' @param MB capacity of micro SD memory card in MB
#' @param MB.d daily file size in MB
#' @param mAH.d Predicted daily energy consumption in mAH
#' @param mAH capacity of installed batteries
#' @return data frame
#' @examples
#' AudioMoth_days(mAH = 2900, mAH.d = 10, MB.d = 173)
#' @export
AudioMoth_days <- function(mAH.d = 90, mAH = 2600, MB = 128*1000, MB.d = 3456) {
  cat("Maximum memory:", floor(MB/MB.d), "days\n")
  cat("Maximum capacity:", floor(mAH/mAH.d), "days\n")
  return(data.frame(mAH.d, mAH, MB, MB.d,
                    memory_days = floor(MB/MB.d),
                    capacity_days =  floor(mAH/mAH.d)))
}

#' AudioMothSetup
#' @param date Date
#' @inheritParams dusk2dawn
#' @inheritParams AudioMoth_days
#' @export
#'
AudioMothSetup <- function(date = Sys.Date(), MB = 128*1000, mAH = 2900, mAH.d, MB.d, lat = 52.032, lon = 8.517, tz = "UTC") {
## info
suncalc::getSunlightTimes(date, lat = lat, lon = lon, tz = tz, keep = c("sunriseEnd", "sunsetStart"))
}

