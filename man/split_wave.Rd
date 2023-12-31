% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_wave.R
\name{split_wave}
\alias{split_wave}
\title{Simple function to split large audio into segments}
\usage{
split_wave(
  path = NULL,
  file = NULL,
  segment = 600,
  downsample = NULL,
  mono = TRUE,
  rescale = NULL,
  discard_input = FALSE
)
}
\arguments{
\item{path}{path.}

\item{file}{file name.}

\item{segment}{segment length in seconds. Default 600.}

\item{downsample}{optional. allows to downsample to a new sampling rate in Hz.}

\item{mono}{logical. By default coerces to mono.}

\item{rescale}{optional. allows to resacale the wav to a new bit rate (e.g., "8", "16", "24").}

\item{discard_input}{logical. Allows to discard input file after transformation. Defaults to \code{FALSE}}
}
\value{
none
}
\description{
Cuts audio file into segments for processing
}
\details{
Optional parameters \code{downsample}, \code{mono} & \code{rescale} require  \code{\link[reticulate]{source_python}} and several python libraries (\code{source_python}, \code{audioop} & \code{wave}).
}
\examples{
\dontrun{
## create test folder
dir.create("test_folder")
## copy example audio
file.copy(from = system.file("extdata", "20211220_064253.wav", package = "NocMigR2"),
         to = "test_folder/20211220_064253.wav")
## rename
x <- rename_recording("test_folder")
x$new.name
## split in segments
split_wave(file = x$new.name, # audio file
           path = "test_folder/", # folder
           segment = 3) # cut in 3 sec segments
## show files
list.files("test_folder/split/")
## delete folder
unlink("test_folder", recursive = T)
}

}
