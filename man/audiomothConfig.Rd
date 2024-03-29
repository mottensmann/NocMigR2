% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{audiomothConfig}
\alias{audiomothConfig}
\title{Read AudioMoth configuration file}
\source{
https://github.com/edwbaker/SonicScrewdriveR/blob/master/R/audiomoth.R
}
\usage{
audiomothConfig(filename)
}
\arguments{
\item{filename}{Path to the configuration file to read}
}
\value{
A data frame of matching annotations
}
\description{
Reads and parses an AudioMoth configuration file.
}
\examples{
\dontrun{
audiomothConfig("./CONFIG.TXT")
}
}
