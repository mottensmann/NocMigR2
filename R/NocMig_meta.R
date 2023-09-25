#' Description of \strong{dusk}, \strong{dawn} and \strong{weather conditions} during NocMig recording session
#' @details
#' (1) Extract dusk and dawn based on date and location using \code{\link[suncalc]{getSunlightTimes}}
#' (2) Retrieve weather data by querying \strong{api.brightsky.dev}
#' (3) Compose two string for lists before and past midnight
#'
#' @note
#' strings follow the format suggested in https://www.hgon.de/fileadmin/HGONContent/03-Beobachten/07-NocMig/NocMig_Methodenstandardisierung_V1.pdf
#'
#' @references
#' Conversion wind direction from degrees to cardinal follows https://community.rstudio.com/t/convert-wind-direction-degrees-into-factors-in-a-data-frame/14636/2
#'
#' Weather is summarised by computing mean (temp, windspeed) or mode (cloud cover, wind direction) during NocMig recording sessions, separately for the time before and after midnight.
#' @param lang Currently only \code{'de'}
#' @inheritParams dusk2dawn
#' @examples
#' # example code
#' NocMig_meta(lat = 52, lon = 8, date = Sys.Date())
#' @export
#'
NocMig_meta <- function(
    lat = 52.032090,
    lon = 8.516775,
    lang = 'de',
    date = Sys.Date() - 1) {

  ## compose URL to query api.brightsky.dev
  ## ---------------------------------------------------------------------------
  URL1 <- paste0(
    'https://api.brightsky.dev/weather?',
    'lat=', lat, '&lon=', lon, '&date=',
    format(date, "%Y-%m-%d"))

  URL2 <- paste0(
    'https://api.brightsky.dev/weather?',
    'lat=', lat, '&lon=', lon, '&date=',
    format(date + 1, "%Y-%m-%d"))

  ## get data and convert to data frame
  ## ---------------------------------------------------------------------------
  df1 <- as.data.frame(jsonlite::fromJSON(URL1)$weather[,c(1:15,17)])
  df2 <- as.data.frame(jsonlite::fromJSON(URL2)$weather[,c(1:15,17)])

  ## merge data frames
  ## ---------------------------------------------------------------------------
  df <- unique.data.frame(rbind(df1, df2))

  ## translate timestamp to datime
  ## ---------------------------------------------------------------------------
  df$date <- lubridate::make_datetime(
    year = as.numeric(substr(df$timestamp, 1, 4)),
    month = as.numeric(substr(df$timestamp, 6, 7)),
    day = as.numeric(substr(df$timestamp, 9, 10)),
    hour = as.numeric(substr(df$timestamp, 12, 13)),
    min = 0,
    sec = 0,
    tz = "CET")

  ## select time between dusk and dawn
  ## ---------------------------------------------------------------------------
  dusk_dawn <- dusk2dawn(date, lat, lon)
  df_dawn_midnight <- dplyr::filter(df, date >= lubridate::round_date(dusk_dawn$dusk, "h"),
                                    date < lubridate::round_date(dusk_dawn$dusk, "d"))
  df_midnight_dusk <- dplyr::filter(df, date >= lubridate::round_date(dusk_dawn$dusk, "d"),
                                    date < lubridate::round_date(dusk_dawn$dawn, "h"))

  ## prepare output
  ## ---------------------------------------------------------------------------
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]}

  ##  Convert wind directions
  ## ---------------------------------------------------------------------------
  rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
  rose_labs <- c(
    "N", "NNE", "NE", "ENE",
    "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW",
    "N")

  # Fri Apr 28 21:24:51 2023 ------------------------------
  # Remove var icon, apparently not embedded in data frame anymore
  #

  out1 <- data.frame(
    cond = dplyr::case_when(
      getmode(df_dawn_midnight$condition) == "rain" ~ "regnerisch",
      getmode(df_dawn_midnight$condition) == "dry" ~ "trocken",
      TRUE ~ getmode(df_midnight_dusk$condition)),
    temp = round(mean(df_dawn_midnight$temperature, na.rm = T),0),
    wind_dir =
      cut(mean(df_dawn_midnight$wind_direction, na.rm = T),
          breaks = rose_breaks,
          labels = rose_labs,
          right = FALSE,
          include.lowest = TRUE
      ),
    wind_speed = round(mean(df_dawn_midnight$wind_speed, na.rm = T),0))

  out2 <- data.frame(
    cond = dplyr::case_when(
      getmode(df_midnight_dusk$condition) == "rain" ~ "regnerisch",
      getmode(df_midnight_dusk$condition) == "dry" ~ "trocken",
      TRUE ~ getmode(df_midnight_dusk$condition)),
    temp = round(mean(df_midnight_dusk$temperature, na.rm = T),0),
    wind_dir =
      cut(mean(df_midnight_dusk$wind_direction, na.rm = T),
          breaks = rose_breaks,
          labels = rose_labs,
          right = FALSE,
          include.lowest = TRUE
      ),
    wind_speed = round(mean(df_midnight_dusk$wind_speed, na.rm = T),0))

  ## output strings
  ## -----------------------------------------------------------------------------
  part1 <- paste0("Teilliste 1: ",
                  dusk_dawn$string, ", ",
                  #out1$icon, "-",
                  out1$cond, ", ",
                  out1$temp, stringi::stri_unescape_unicode("\\u00b0"), "C", ", ",
                  out1$wind_dir, ", ",
                  out1$wind_speed, " km/h")
  if (stringr::str_detect(part1, "regnerisch-regnerisch")) {
    part1 <- stringr::str_replace(part1, "regnerisch-regnerisch", "regnerisch")
  }
  part2 <- paste0("Teilliste 2: ",
                  dusk_dawn$string, ", ",
                  #out2$icon, "-",
                  out2$cond,", ",
                  out2$temp, stringi::stri_unescape_unicode("\\u00b0"), "C", ", ",
                  out2$wind_dir, ", ",
                  out2$wind_speed, " km/h")
  if (stringr::str_detect(part2, "regnerisch-regnerisch")) {
    part2 <- stringr::str_replace(part2, "regnerisch-regnerisch", "regnerisch")
  }
  cat(part1,"\n")
  cat(part2)
  df <- (data.frame(part1 = part1, part2 = part2))
}
