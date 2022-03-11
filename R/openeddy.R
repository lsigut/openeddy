#' Post-process eddy covariance data with ease
#'
#' The openeddy package is a software infrastructure for eddy covariance data
#' post-processing. It provides tools for handling data with attached metadata,
#' setting up quality control scheme and summarizing and plotting the results.
#' It is aligned with REddyProc package that provides methods for
#' uStar-filtering, gap-filling, and flux-partitioning. Thus the combined use of
#' openeddy and REddyProc allows to run the whole eddy covariance
#' post-processing chain. Learn more at <https://github.com/lsigut/EC_workflow>.
#'
#' @section References: Mauder, M., Cuntz, M., Drue, C., Graf, A., Rebmann, C.,
#'   Schmid, H.P., Schmidt, M., Steinbrecher, R., 2013. A strategy for quality
#'   and uncertainty assessment of long-term eddy-covariance measurements.
#'   Agric. For. Meteorol. 169, 122-135. doi:10.1016/j.agrformet.2012.09.006
#'
#' @docType package
#' @name openeddy
#'
#' @importFrom stats median na.omit quantile aggregate
NULL