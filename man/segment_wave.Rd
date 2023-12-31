% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segment_wave.R
\name{segment_wave}
\alias{segment_wave}
\title{segment wave files}
\usage{
segment_wave(path, length = 30, format = c("wav", "mp3"), keep.input = FALSE)
}
\arguments{
\item{path}{path}

\item{length}{length of individuals audio segments in seconds, only accepting multiples of three. Defaults to 30}

\item{format}{audio format. Supported are \code{'wav'} and \code{'mp3'}}

\item{keep.input}{logical. defaults to TRUE}
}
\description{
Extracts segments of a user defined length from input files. Input files are either moved to a subfolder (\code{keep.input = TRUE}) or deleted after the processing is executed.
}
