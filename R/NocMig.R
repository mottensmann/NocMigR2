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
  URL1 <- paste0(
    'https://api.brightsky.dev/weather?',
    'lat=', lat, '&lon=', lon, '&date=',
    format(date, "%Y-%m-%d"))

  URL2 <- paste0(
    'https://api.brightsky.dev/weather?',
    'lat=', lat, '&lon=', lon, '&date=',
    format(date + 1, "%Y-%m-%d"))

  ## get data and convert to data frame
  df1 <- try(as.data.frame(jsonlite::fromJSON(URL1)$weather[,c(1:15,17)]))
  df2 <- try(as.data.frame(jsonlite::fromJSON(URL2)$weather[,c(1:15,17)]))

  if (methods::is(df1, "try-error") | methods::is(df2, "try-error")) {
    ## try again after sleeping for a few seconds to avoid server issues
    cat("api.brightsky did not respond. Retry in 10s ...")
    Sys.sleep(time = 10)
    df1 <- (as.data.frame(jsonlite::fromJSON(URL1)$weather[,c(1:15,17)]))
    df2 <- (as.data.frame(jsonlite::fromJSON(URL2)$weather[,c(1:15,17)]))
    cat("done")
  }

  ## merge data frames
  df <- unique.data.frame(rbind(df1, df2))

  ## translate timestamp to datime
  df$date <- lubridate::make_datetime(
    year = as.numeric(substr(df$timestamp, 1, 4)),
    month = as.numeric(substr(df$timestamp, 6, 7)),
    day = as.numeric(substr(df$timestamp, 9, 10)),
    hour = as.numeric(substr(df$timestamp, 12, 13)),
    min = 0,
    sec = 0,
    tz = "CET")

  ## select time between dusk and dawn
  dusk_dawn <- dusk2dawn(date, lat, lon)
  df_dawn_midnight <- dplyr::filter(df, date >= lubridate::round_date(dusk_dawn$dusk, "h"),
                                    date < lubridate::round_date(dusk_dawn$dusk, "d"))
  df_midnight_dusk <- dplyr::filter(df, date >= lubridate::round_date(dusk_dawn$dusk, "d"),
                                    date < lubridate::round_date(dusk_dawn$dawn, "h"))

  ## prepare output
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]}

  ##  Convert wind directions
  rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
  rose_labs <- c(
    "N", "NNE", "NE", "ENE",
    "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW",
    "N")

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
#' Process NocMig session for downstream analysis
#'
#' @param parent.folder path
#' @param child.folder optional. defaults to an empty string \code{''}
#' @param rename logical. defaults to \code{TRUE}
#' @param segment_length optional numeric value. If specified, function \code{\link{split_wave}} is used to to segmenti long audio files in shorter segments. defaults to \code{NULL}
#' @inheritParams rename_recording
#' @inheritParams NocMig_meta
#' @export
#'
NocMig_process <- function(
    parent.folder = "E:/NocMig",
    child.folder = "",
    format = c("wav", "mp3"),
    rename = TRUE,
    time_reference = c("first", "each"),
    ctime = TRUE,
    lat = 52.032090,
    lon = 8.516775,
    segment_length = NULL) {

  format <- match.arg(format)
  time_reference <- match.arg(time_reference)

  if (!dir.exists(file.path(parent.folder, child.folder))) {
    cat(file.path(parent.folder, child.folder), "not found!\n")
  }

  ## 02: Rename Recording (not touching files with proper name!)
  if (isTRUE(rename)) {
    cat("Rename files in", file.path(parent.folder, child.folder), "\n")
    output <- rename_recording(
      path = file.path(parent.folder, child.folder),
      ctime = ctime,
      time_reference = time_reference,
      format = format)
    output.time <- RecreateDateTime(output$new.name)
  } else {
    output <- list.files(path = file.path(parent.folder, child.folder),
                         pattern = paste0(".", format),
                         full.names = F,
                         ignore.case = T)
    output.time <- RecreateDateTime(output)
  }

  ## 03: Obtain NocMig header
  cat("Write NocMig head to file", file.path(parent.folder, child.folder, "NocMig_meta.txt"), "\n")
  sink(file.path(parent.folder, child.folder, "NocMig_meta.txt"))
  NocMig_meta(lat = lat, lon = lon, date = lubridate::as_date(min(output.time)))
  sink()

  if (!is.null(segment_length) & methods::is(segment_length, "numeric")) {
    if (format == "mp3") {
      stop("Segementing audio files required wave files - mp3 is not supported")
    } else {
      audio.files <- list.files(file.path(parent.folder, child.folder),
                                pattern = format, ignore.case = TRUE)
      lapply(audio.files, split_wave,
             path = file.path(parent.folder, child.folder),
             segment = segment_length, discard_input = T)
    }

  }
}

#' Compose dusk-to-dawn string based on date and location
#'
#' @description
#' Calling \code{\link[suncalc]{getSunlightTimes}} to retrieve times of dusk and dawn for a coordinate pair
#'
#' @param date Date. Default \code{\link[base]{Sys.Date}}
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param tz timezone. Default CEt
#' @examples
#' ## usage
#' dusk2dawn(date = Sys.Date(), lat = 52, lon = 8)
#' @export
#'
dusk2dawn <- function(date = Sys.Date(), lat = 52.032, lon = 8.517, tz = "CET") {
  d1 <- suppressWarnings(suncalc::getSunlightTimes(
    date = date, lat = lat, lon = lon, tz = tz, keep = "dusk"))
  d2 <- suppressWarnings(suncalc::getSunlightTimes(
    date = date + 1, lat = lat, lon = lon, tz = tz, keep = "dawn"))
  return(data.frame(
    dusk = d1$dusk,
    dawn = d2$dawn,
    string = paste0(
      lubridate::day(d1$date), ".", lubridate::month(d1$date), "-",
      lubridate::day(d2$date), ".", lubridate::month(d2$date), ".", lubridate::year(d2$date), ", ",
      substr(as.character(d1$dusk), 12, 16), "-", substr(as.character(d2$dawn), 12, 16)
    )
  ))

}





