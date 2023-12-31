% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename_recording.R
\name{rename_recording}
\alias{rename_recording}
\title{Rename recording with a string of the form YYYYMMDD_HHMMSS}
\usage{
rename_recording(
  path = NULL,
  time_reference = c("first", "each"),
  format = c("wav", "mp3"),
  ctime = TRUE,
  .simulate = FALSE
)
}
\arguments{
\item{path}{Path to folder containing audio files}

\item{time_reference}{(1) Specify \code{'first'} when all recordings share ctime/mtime of the first recording (e.g. \strong{Olympus LS recorder}). (2) Specify \code{'each'} when recordings have unique ctime/mtime (e.g. \strong{Sony PCM recorder})}

\item{format}{audio format. Supported are \code{'wav'} and \code{'mp3'}}

\item{ctime}{logical. defaults to \code{TRUE}. If \code{FALSE} mtime is used instead.}

\item{.simulate}{Logical. Default \code{FALSE}}
}
\value{
TRUE if successful
}
\description{
Rename recordings using time-stamps derived from \code{ctime} of the audio file.
}
\details{
Currently renaming distinguishes two cases:
   (1) \code{time_reference = 'first'}: Deriving time based on *ctime* (or *mtime*) of first recording and compute for subsequent recordings based on recording duration.
   (2) \code{time_reference = 'each'}: Deriving time based on *ctime* (or *mtime*) of each recording separately

In order to figure out the correct strategy for recorders not listed here (or when in doubt), make sure to check file properties carefully and run this function with setting \code{.simulate = TRUE}. The choice of *ctime* vs *mtime* might vary depending on the used computer.
}
\examples{
# example code
rename_recording(
path = system.file("extdata", package = "NocMigR2"),
format = "wav",
.simulate = TRUE)

}
