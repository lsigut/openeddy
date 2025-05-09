% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_checking.R
\name{flag_runs}
\alias{flag_runs}
\title{Flag Runs of Equal Values}
\usage{
flag_runs(x, name_out = "-", length = 4)
}
\arguments{
\item{x}{A numeric \link{atomic} type with \code{NULL} dimensions.}

\item{name_out}{A character string providing \code{varnames} attribute value
of the output.}

\item{length}{A numeric value. The minimum number of repeating values to
trigger flagging.}
}
\value{
An integer vector with the same length as \code{x}. Its
\code{varnames} and \code{units} attributes are set to  \code{name_out} and
\code{"-"} values, respectively.
}
\description{
Identify and flag values of runs with repeating values in a vector.
}
\details{
\code{NA} values are omitted before evaluation of runs. Thus \code{NA}s do
not interrupt runs. Flagging is done according to the 0 - 2 quality control
flag scheme.
}
\examples{
(xx <- c(rep(c(0, NA), 5)))
flag_runs(xx, "qc_xx_runs")
(yy <- rep(1:6, rep(c(2, 1), 3)))
flag_runs(yy, "qc_yy_runs")

}
