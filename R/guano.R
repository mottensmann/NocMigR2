#' Read GUANO metadata
#'
#' @description
#' Originally written by David A. Riggs (https://github.com/riggsd/guano-r) and further developed by WavX, inc. (https://github.com/WavX/bioacoustics/)
#'
#' @param file Path to a wav file
#' @return list of named metadata fields
#' @keywords internal
#'
guano <- function(file) {

  f <- file(file, "rb")
  on.exit(close(f))

  RIFF <- readChar(f, 4)

  if (length(RIFF) == 0 || RIFF != "RIFF") stop("This does not seem to be a valid wav file.")

  riff.size <- readBin(f, integer(), size = 4, endian = "little")
  WAVE <- readChar(f, 4)  # "WAVE"

  if (length(WAVE) == 0 || WAVE != "WAVE") stop("This does not seem to be a valid wav file.")

  read.subchunk <- function() {
    id <- tryCatch(readChar(f, 4), error = function(e) {return("")})

    if (length(id) == 0 || id == "") return(NULL)

    size <- readBin(f, integer(), size = 4, endian = "little")
    list(id = id, size = size)
  }

  skip.subchunk <- function(chunk) {
    pos <- seek(f, NA)
    seek(f, pos + chunk$size)
  }

  .parse.timestamp <- function(str) {
    if (is.na(str) || is.null(str) || str == "") {
      return(NA)
    } else if (endsWith(str, "Z")) {
      # UTC
      return(strptime(str, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    } else if (length(gregexpr(":", str)[[1]]) == 3) {
      # UTC offset
      len <- nchar(str)
      str <- paste(substr(str, 1, len-3), substr(str, len-1, len), sep = "")
      return(strptime(str, "%Y-%m-%dT%H:%M:%S%z"))
    }  else {
      # local
      return(strptime(str, "%Y-%m-%dT%H:%M:%S"))
    }
  }

  # Maps metadata keys to a data type coercion function
  data_types <- list(
    `Filter HP` = as.double,
    `Filter LP` = as.double,
    Humidity = as.double,
    Length = as.double,
    `Loc Accuracy` = as.integer,
    `Loc Elevation` = as.double,
    Note = function(val) gsub("\\\\n", "\n", val),
    Samplerate = as.integer,
    TE = function(val) if (is.na(val) || is.null(val) || val == "") 1 else as.integer(val),
    `Temperature Ext` = as.double,
    `Temperature Int` = as.double,
    Timestamp = .parse.timestamp)

  md <- list()

  while (!is.null(chunk <- read.subchunk())) {
    if (chunk$id != "guan") {
      skip.subchunk(chunk)
      next
    }

    md.txt <- readChar(f, chunk$size)

    for (line in strsplit(md.txt, "\n")[[1]]) {
      line <- trimws(line)
      if (line == "") next

      toks <- strsplit(sub(":", "\n", line), "\n")
      key <- trimws(toks[[1]][1])
      val <- trimws(toks[[1]][2])

      if (is.na(key) || is.null(key) || key == "") next

      if (!is.null(data_types[[key]])) val <- data_types[[key]](val)

      md[[key]] <- val
    }

    if ("Loc Position" %in% names(md)) {
      coords <- lapply(strsplit(md[["Loc Position"]], " "), as.double)[[1]]
      md[["Loc Position Lat"]] <- coords[1]
      md[["Loc Position Lon"]] <- coords[2]
      md[["Loc Position"]] <- NULL
    }
  }
  return(md)
}
