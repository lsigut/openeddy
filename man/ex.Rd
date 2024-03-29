% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data_handling.R
\name{ex}
\alias{ex}
\title{Extract Parts of an Object with Varnames and Units Attributes}
\usage{
ex(x, i, j, drop = TRUE)
}
\arguments{
\item{x}{An atomic type or a data frame. Object from which to extract
element(s).}

\item{i, j}{Indices specifying elements to extract as specified in
\code{\link{Extract}}.}

\item{drop}{A logical value. If \code{TRUE} (default), the result is coerced
to the lowest possible dimension.}
}
\value{
A vector or data frame with \code{varnames} and \code{units}
  attributes.
}
\description{
Conserves \code{varnames} and \code{units} attributes of vectors and data
frames during extraction.
}
\details{
Extraction from atomic types is done as \code{x[i]} ignoring \code{j} and
\code{drop} (applies also to matrices and arrays). Extraction from data
frames is done as \code{x[i, j, drop]}.
}
\examples{
xx <- data.frame(lengths = 1:3, areas = 4:6)
varnames(xx) <- c("lengths", "areas")
units(xx) <- c("m", "m2")
str(xx)

# extract specified rows and columns
str(ex(xx, 1:2, 1:2))
# extract specified rows
str(ex(xx, 1))
# extract specified columns
str(ex(xx, , 1))
# extract without dropping current class
ex(xx, , 1, drop = FALSE)
# extract elements of a vector
ex(xx$lengths, 2:3)

}
\seealso{
\code{\link{Extract}}, \code{\link{drop}} and
  \code{\link{varnames}}.
}
