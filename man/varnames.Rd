% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_handling.R
\name{varnames}
\alias{varnames}
\alias{varnames<-}
\alias{units}
\alias{units<-}
\title{Object Attributes Varnames and Units}
\usage{
varnames(x, names = FALSE)

varnames(x) <- value

units(x, names = FALSE)

units(x) <- value
}
\arguments{
\item{x}{A data frame or an \link{atomic} type.}

\item{names}{A logical value. Applies only in case of data frames. If
\code{TRUE}, attributes are extracted with corresponding column names.}

\item{value}{An \link{atomic} type that represents \code{varnames} or \code{units}.
The length must be \code{1} if \code{x} is an atomic type or equal to
number of columns in \code{x} if \code{x} is a data frame.}
}
\value{
For \code{varnames} and \code{units}, a character vector.

For \verb{varnames<-} and \verb{units<-}, the updated object \code{x}.
}
\description{
\code{varnames} and \code{units} are useful attributes that can store
original variable names (\code{varnames}) and units of measurement
(\code{units}) of each column in a data frame or of an atomic type. These
attributes can be extracted or assigned by following functions.
}
\details{
Functions check whether the extracted or assigned attributes contain elements
with \code{NULL}, \code{NA}, \code{""} values or if length of each element is
higher than \code{1}. In these cases, such elements are substituted with
\code{"-"}.
}
\examples{
xx <- data.frame(a = 1, b = 2, c = 3, d = 4)
lapply(xx, attr, "units")
units(xx, names = TRUE)
varnames(xx) <- c("a", "", NA, "d")
units(xx) <- 1:4
str(xx)
units <- units(xx)

## NB: subsetting by rows removes 'varnames' and 'units' attributes
str(yy <- xx[1, ])
varnames(yy) <- names(yy)
units(yy) <- units
str(yy)
}
\seealso{
\code{\link[=read_eddy]{read_eddy()}} and \code{\link[=write_eddy]{write_eddy()}}.
}
