#' Compute AudioMoth recording days
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

  # constants <- data.frame(
  #   sampleRate = c(8,16,32,48,96,192,250,384),
  #   sampleRateDivider = c(48,24,12,8,4,2,1,1)
  # )

  # ## File size from duration and sample rate
  # getFileSize <- function(sampleRate, sampleRateDivider, secs) {
  #   return((sampleRate / sampleRateDivider) * 2 * secs)
  # }
  # formatFileSize <- function(fileSize) {
  #   fileSize <- round(fileSize / 1000)
  #   if (fileSize < 10000) {
  #     return(paste0(fileSize, ' kB'))
  #   } else {
  #     fileSize <- round(fileSize / 1000)
  #     if (fileSize < 10000) {
  #       return(paste0(fileSize, ' MB'))
  #     } else {
  #       fileSize <- round(fileSize / 1000)
  #       return(paste0(fileSize, ' GB'))
  #
  #     }
  #   }
  # }

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


# AudioMoth_days(mAH = 2900, mAH.d = 10, MB.d = 173)
# AudioMoth_days(mAH = 2900, mAH.d = 240, MB.d = 6221)
# AudioMoth_days(mAH = 2900, mAH.d = 200, MB.d = 5288)
# AudioMoth_days(mAH = 2900, mAH.d = 220, MB.d = 6019)
# AudioMoth_days(mAH = 2750, mAH.d = 200, MB.d = 5386, MB = 32*1000)
