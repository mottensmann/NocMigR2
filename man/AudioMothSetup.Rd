% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AudioMothSetup.R
\name{AudioMothSetup}
\alias{AudioMothSetup}
\title{AudioMothSetup}
\usage{
AudioMothSetup(
  date = Sys.Date(),
  MB = 128 * 1000,
  mAH = 2900,
  mAH.d,
  MB.d,
  lat = 52.032,
  lon = 8.517,
  tz = "UTC"
)
}
\arguments{
\item{date}{Date}

\item{MB}{capacity of micro SD memory card in MB}

\item{mAH}{capacity of installed batteries}

\item{mAH.d}{Predicted daily energy consumption in mAH}

\item{MB.d}{daily file size in MB}

\item{lat}{latitude in decimal degrees}

\item{lon}{longitude in decimal degrees}

\item{tz}{timezone. Default CEt}
}
\description{
AudioMothSetup
}
