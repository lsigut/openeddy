% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_checking.R
\name{fetch_filter}
\alias{fetch_filter}
\title{Apply Fetch Filter}
\usage{
fetch_filter(x, fetch_name, wd_name, ROI_boundary, name_out = "-")
}
\arguments{
\item{x}{A data frame with column names representing required variables.}

\item{fetch_name}{A character string. Specifies the column name in \code{x}
with fetch distance values.}

\item{wd_name}{A character string. Specifies the column name in \code{x} with
wind direction values.}

\item{ROI_boundary}{A numeric vector. Represents the boundary of region of
interest.}

\item{name_out}{A character string providing \code{varnames} attribute value
of the output.}
}
\value{
An integer vector with attributes \code{"varnames"} and
\code{"units"}.
}
\description{
\code{fetch_filter} flags all halfhours that have longer fetch distance (of
given percentage of contribution to the flux) than the user defined boundary
of the region of interest (ROI).
}
\details{
Fetch distance is used together with wind direction information to identify
the cases when fetch reached beyond ROI.

The spatial extent of the studied ecosystem (ROI) is specified by its
\code{ROI_boundary} that describes the distance from eddy covariance tower to
the edge of the studied ecosystem. \code{ROI_boundary} has following
properties:
\itemize{
\item the number of circular sectors is the same as the
number of provided distances;
\item the angular resolution of the ROI
boundary can be computed as 360 degrees / number of angular sectors;
\item the ROI boundary distances are assigned to the centers of their respective
circular sectors with first sector centered on 0 degrees.
}

Example: \code{ROI_boundary} specified as c(150, 200, 250, 300) has following
properties:
\itemize{
\item 4 circular sectors with 90° angular resolution;
\item ROI boundary is specified for the whole first sector (315°, 45°] at the
distance 150 m from tower (center of the sector is 0°);
\item boundary of the second sector (45°, 135°] is at the distance 200 m;
\item third sector (135°, 225°] is at the distance 250 m;
\item fourth sector (225°, 315°] is at the distance 300 m.
}
}
\examples{
set.seed(20)
(xx <- data.frame(x_70perc = sample(1:1000, 10), WD = sample(0:360, 10)))
fetch_filter(xx, "x_70perc", "WD", 300, "qc_ALL_fetch70")

}
\seealso{
\code{\link[=combn_QC]{combn_QC()}} and \code{\link[=extract_QC]{extract_QC()}}.
}
