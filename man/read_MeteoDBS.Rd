% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_handling.R
\name{read_MeteoDBS}
\alias{read_MeteoDBS}
\title{Read Meteorological Data with Units}
\usage{
read_MeteoDBS(
  path,
  start = NULL,
  end = NULL,
  format = "\%d.\%m.\%Y \%H:\%M",
  shift.by = NULL,
  allow_gaps = TRUE,
  verbose = TRUE,
  pattern = "\\\\.[Cc][Ss][Vv]$"
)
}
\arguments{
\item{path}{A string. The path to directory with CSV file(s) in Czechglobe
MeteoDBS format. Other than CSV files are ignored.}

\item{start, end}{A value specifying the first (last) value of the generated
date-time sequence in temporary column "timestamp". If \code{NULL}, \code{\link[=min]{min()}}
(\code{\link[=max]{max()}}) of date-time values from "date/time" column across all files is
used. If numeric, the value specifies the year for which the first (last)
date-time value will be generated, considering given time interval
(automatically detected from "date/time" column) and convention of
assigning of measured records to the end of the time interval. Otherwise,
character representation of specific date-time value is expected in given
\code{format} and timezone "GMT".}

\item{format}{A character string. Format of \code{start} (\code{end}) if provided as a
character string.}

\item{shift.by}{A numeric value specifying the time shift (in seconds) to be
applied to the date-time information.}

\item{allow_gaps}{A logical value. If \code{TRUE}, date-time information does not
have to be regular but time differences must be multiples of automatically
detected time interval.}

\item{verbose}{A logical value. Should additional statistics about presence
of \code{NA} values in resulting data frame be printed to console?}

\item{pattern}{A character string. A \link[=regexp]{regular expression} \code{\link[=grep]{grep()}}
\code{pattern} identifying MeteoDBS files in the \code{path} folder.}
}
\value{
A data frame is produced with additional attributes \code{varnames} and
\code{units} assigned to each respective column.
}
\description{
Read single or multiple meteorological data files at Czechglobe MeteoDBS
format at given path and merge them together along generated regular
date-time sequence.
}
\details{
This utility function is adapted to Czechglobe MeteoDBS file structure but
allows to change selected useful arguments that have preset default values.
It also assures that date-time sequence is regular and equidistant.

In case that multiple files are present in the \code{path}, the expectation is
that files represent meteorological variables for given site and different
periods. Function merges them vertically (along generated complete
timestamp). All original columns across all files excluding the last empty
one are kept. The order of variables keeps that of the first file loaded
(note that file ordering in \code{path} is alphabetical not chronological) and
additional variables are appended if present in the following files. The
output "date/time" column is converted into class \code{POSIXct}.

If you want to specify \code{start} and \code{end} arguments as strings and you change
also default \code{shift.by} value, \code{start} and \code{end} arguments need to be adopted
accordingly to account for that change. E.g. if \code{shift.by = -900}, then
\verb{start = "2019-12-31 21:15:00", end = "2019-12-31 23:15:00"} instead of
\verb{start = "2019-12-31 21:30:00", end = "2019-12-31 23:30:00"} for half-hourly
data.

Function introduces additional column "timestamp" for purposes of merging
with \code{merge_eddy()}. This column is then removed as it is not included in the
original data.
}
\examples{
# examples of different patterns for file selection
xx <- c("CZ-BK1_2024_meteo.csv", "data.CSV", "CZ-BK1.txt")

# select file names ending with ".csv" (case insensitive)
grep("\\\\.[Cc][Ss][Vv]$", xx, value = TRUE)

# select file names starting with CZ-BK1 site abbreviation
grep("^CZ-BK1", xx, value = TRUE)

# select CSV file names starting with CZ-BK1 site abbreviation
# - note the usage of ".*" to combine above patterns
grep("^CZ-BK1.*\\\\.[Cc][Ss][Vv]$", xx, value = TRUE)

}
