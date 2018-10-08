\name{openeddy-package}
\alias{openeddy-package}
\alias{openeddy}
\title{Utilities for eddy covariance data handling in R}
\description{
Utilities for eddy covariance data handling, including
\itemize{
\item quality checking (similar to Mauder et al., 2013),
\item processing,
\item summarising,
\item and plotting.
}
It aims to standardise the automated quality checking and make data
processing reproducible.
}%description


\details{

Data processing
\itemize{
 \item{Storage correction: \code{\link{add_st}} }
 \item{Low frequency flux despiking: \code{\link{despikeLF}} }
 \item{Filtering for region of interest: \code{\link{fetch_filter}} }
 \item{Interdependency of H, LE and NEE QC flags due to
 corrections/conversionst: \code{\link{interdep}} }
 }

Utilities
 \itemize{
 \item{ reading and write tabular data:
 \code{\link{read_eddy}}, \code{\link{write_eddy}} }
 \item{ creating POSIX time column: \code{\link{strptime_eddy}} }
 \item{ combine quality checking results: \code{\link{combn_QC}} }
 }

Keeping track of names and units of variables
 \itemize{
 \item{variables, i.e. names of columns:
   \code{\link{varnames}} }
 \item{associated units:
   \code{\link{units}} }
 }

Plotting
 \itemize{
 \item{Visualize flux measurements together with micrometeorological or
 other related variables in monthly and weekly intervals.:
 \code{\link{plot_eddy}} }
 }

}%details

\references{
Mauder, M., Cuntz, M., Drüe, C., Graf, A., Rebmann, C., Schmid, H.P., Schmidt,
M., Steinbrecher, R., 2013.
A strategy for quality and uncertainty assessment of long-term
eddy-covariance measurements.
Agric. For. Meteorol. 169, 122-135,
https://doi.org/10.1016/j.agrformet.2012.09.006
}

\author{Ladislav Sigut, Thomas Wutzler}

\keyword{package}


