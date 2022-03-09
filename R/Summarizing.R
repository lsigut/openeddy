#' Time series summarization
#'
#' Utilities that simplify aggregation of data and their uncertainties over
#' defined time intervals.
#'
#' \code{agg_mean} and \code{agg_sum} compute mean and sum over intervals
#' defined by \code{format} and/or \code{breaks} for all columns.
#'
#' \code{agg_fsd} and \code{agg_DT_SD} estimate aggregated mean and summed
#' uncertainties over defined time periods for \code{REddyProc} package
#' gap-filling and daytime-based flux partitioning outputs, respectively. The
#' uncertainty aggregation accounts for autocorrelation among records. It is
#' performed only for autodetected columns with appropriate suffixes (see
#' further). Note that uncertainty products of \code{agg_fsd} and
#' \code{agg_DT_SD} are reported as standard deviations (\code{SD}) and require
#' further correction to represent uncertainty bounds for given confidence
#' interval (e.g. \code{SD * 1.96} for 95\% confidence level).
#'
#' The summarizations are done on a data frame \code{x} with required timestamp
#' column (\code{x$timestamp}) of class \code{"POSIXt"}. With exception of
#' \code{agg_mean}, the timestamp must form regular sequence without \code{NA}s
#' due to time resolution estimation.
#'
#' Change of aggregation interval can be achieved through \code{breaks} and
#' \code{format} arguments.
#'
#' The data frame \code{x} can be \link[=cut.POSIXt]{cut} to custom intervals
#' using argument \code{breaks}. Note that labels are constructed from the
#' left-hand end of the intervals and converted to \code{"POSIXct"} class. This
#' can be useful when aggregating e.g. half-hourly data over hourly
#' (\code{breaks = "60 mins"}) or three-day (\code{breaks = "3 days"})
#' intervals.
#'
#' The formatting of the timestamp (original or after cutting) using
#' \code{format} is another (preferable) way to change aggregation intervals.
#' For example changing original \code{"POSIXt"} time format (\code{"\%Y-\%m-\%d
#' \%H:\%M:\%S"}) to \code{"\%Y-\%m-\%d"}, \code{"\%W_\%y"}, \code{"\%m-\%y"} or
#' \code{"\%Y"} will result in daily, weekly, monthly or yearly aggregation
#' intervals, respectively. Note that improper \code{format} can repress
#' expected effect of \code{breaks}.
#'
#' \code{agg_fsd} and \code{agg_DT_SD} require certain columns with defined
#' suffixes in order to evaluate uncertainty correctly. These columns are a
#' product of \code{REddyProc} package gap-filling and flux partitioning methods
#' and are documented here:
#' \url{https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWebOutput}.
#' Detailed description of uncertainty aggregation is available here:
#' \url{https://github.com/bgctw/REddyProc/blob/master/vignettes/aggUncertainty.md}.
#'
#' \code{agg_fsd} requires columns with suffixes \code{_fall}, \code{_orig},
#' \code{_fqc} and \code{_fsd} for each variable.
#'
#' \code{agg_DT_SD} requires corresponding columns with \code{\link{regexp}}
#' patterns \code{"^NEE_.*_orig$"}, \code{"^NEE_.*_fqc$"}, \code{"^Reco_DT_"},
#' \code{"^GPP_DT_"}, \code{"^Reco_DT_.*_SD$"} and \code{"^GPP_DT_.*_SD$"}.
#'
#' @section Unit Conversion: In case of aggregation using \code{sum}, i.e.
#'   \code{agg_sum}, \code{agg_fsd} and \code{agg_DT_SD}, appropriate unit
#'   conversion can be applied to columns defined by \code{quant}, \code{power},
#'   \code{carbon} and \code{ET} arguments. The conversion factor used for
#'   approximate PAR conversion from umol m-2 s-1 to W m-2 is 4.57 as proposed
#'   by Thimijan and Heins (1983; Tab. 3, Lightsource - Sun and sky, daylight).
#'
#' @section Sign Correction: Although the sign convention used for measured NEE
#'   (Net Ecosystem Exchange) typically denotes negative fluxes as CO2 uptake,
#'   summed NEE is typically reported with the opposite sign convention and is
#'   assumed to converge to NEP (Net Ecosystem Production), especially over
#'   longer aggregation intervals. Similarly, estimated negative GPP (Gross
#'   Primary Production) typically denotes carbon sink but should be corrected
#'   to positive values if summed over a time period.
#'
#'   There is no reliable way to guess the sign convention used in the data set.
#'   Thus \code{agg_sum} allows to specify whether NEE (\code{NEE_scor}) and/or
#'   GPP (\code{GPP_scor}) sign correction is required. By default
#'   \code{NEE_scor = TRUE} and \code{GPP_scor = FALSE} considering sign
#'   conventions used in \code{REddyProc} package. \code{agg_sum} automatically
#'   detects all NEE and GPP columns in \code{x} using regular expressions and
#'   applies the sign correction settings.
#'
#' @section References: Bayley, G. and Hammersley, J., 1946. The "Effective"
#'   Number of Independent Observations in an Autocorrelated Time Series.
#'   Supplement to the Journal of the Royal Statistical Society, 8(2), 184-197.
#'   doi: \url{https://doi.org/10.2307/2983560}
#'
#'   Thimijan, R.W. and Heins R.D., 1983. Photometric, Radiometric, and Quantum
#'   Light Units of Measure: A Review of Procedures for Interconversion.
#'   Horticultural Science, Vol. 18(6), 818-822.
#'
#'   Zieba, A. and Ramza, P., 2011. Standard Deviation of the Mean of
#'   Autocorrelated Observations Estimated with the Use of the Autocorrelation
#'   Function Estimated From the Data. Metrology and Measurement Systems, 18(4),
#'   529-542. doi: \url{https://doi.org/10.2478/v10178-011-0052-x}
#'
#' @param x A data frame with required timestamp column (\code{x$timestamp}) of
#'   class \code{"POSIXt"}.
#' @param format A character string specifying \code{x$timestamp} formatting for
#'   aggregation through internal \code{\link{strftime}} function.
#' @param breaks A vector of cut points or number giving the number of intervals
#'   which \code{x$timestamp} is to be cut into or an interval specification,
#'   one of \code{"sec"}, \code{"min"}, \code{"hour"}, \code{"day"},
#'   \code{"DSTday"}, \code{"week"}, \code{"month"}, \code{"quarter"} or
#'   \code{"year"}, optionally preceded by an integer and a space, or followed
#'   by \code{"s"}.
#' @param interval A numeric value specifying the time interval (in seconds) of
#'   the generated date-time sequence. If \code{NULL}, \code{interval}
#'   autodetection is attempted.
#' @param tz A character string specifying the time zone to be used for the
#'   conversion. System-specific (see \code{\link{as.POSIXlt}} or
#'   \code{\link{timezones}}), but \code{""} is the current time zone, and
#'   \code{"GMT"} is UTC. Invalid values are most commonly treated as UTC, on
#'   some platforms with a warning.
#' @param ... Further arguments to be passed to the internal
#'   \code{\link{aggregate}} function.
#' @param agg_per A character string providing the time interval of aggregation
#'   that will be appended to units (e.g. \code{"hh-1"}, \code{"week-1"} or
#'   \code{"month-1"}).
#' @param NEE_scor,GPP_scor A logical value. Should sign correction of NEE (GPP)
#'   be performed? See Sign Correction in Details.
#' @param quant A character vector listing variable names that require
#'   conversion from quantum to energy units before aggregation.
#' @param power A character vector listing variable names that require
#'   conversion from power to energy units before aggregation.
#' @param carbon A character vector listing variable names that require
#'   conversion from CO2 concentration to C mass flux units before aggregation.
#' @param ET A character vector listing variable names that require conversion
#'   from hourly interval to actual measurement interval before aggregation.
#'   Designed for evapotranspiration (ET) typically reported in mm hour-1 for
#'   half-hourly measurements.
#'
#' @return \code{agg_mean} and \code{agg_sum} produce a data frame with
#'   attributes varnames and units assigned to each respective column.
#'
#'   \code{agg_fsd} and \code{agg_DT_SD} produce a list with two data frames
#'   \code{mean} and \code{sum} with attributes varnames and units assigned to
#'   each respective column or \code{NULL} value if required columns are not
#'   recognized.
#'
#'   Each produced data frame has first column called "Intervals" with vector of
#'   labels describing aggregation period provided as factor, and second column
#'   "days" providing fraction (or multiple) of days aggregated within each
#'   period.
#'
#' @seealso \code{\link{aggregate}}, \code{\link{as.POSIXlt}},
#'   \code{\link{cut.POSIXt}}, \code{\link{mean}}, \code{\link{regexp}},
#'   \code{\link{strftime}}, \code{\link{sum}}, \code{\link{timezones}},
#'   \code{\link{varnames}}
#'
#' @examples
#' \dontrun{
#'
#' library(REddyProc)
#' library(bigleaf)
#'
#' # Load example dataset from REddyProc package and use selected variables
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
#' Day = 'DoY', Hour = 'Hour')[-(2:4)]
#' EProc <- sEddyProc$new('DE-Tha', DETha98,
#' c('NEE', 'LE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#' names(DETha98)[1] <- "timestamp"
#'
#' # Center timestamp to represent the middle of the averaging period
#' # - necessary for reliable data aggregation
#' DETha98$timestamp <- DETha98$timestamp - 60*15
#'
#' # Aggregate by averaging
#' # - by default any NA value in an aggregation period produces NA
#' agg_mean(DETha98, "%b-%y")
#' agg_mean(DETha98, "%b-%y", na.rm = TRUE)
#'
#' # Aggregate by summation
#' # - sign and unit conversions are demonstrated
#' (zz <- agg_sum(DETha98, "%b-%y", agg_per = "month-1"))
#' openeddy::units(zz, names = TRUE)
#'
#' # Gap-fill NEE using approximate fixed uStar threshold
#' EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
#'
#' # Gap-fill all other selected variables
#' for (i in c('LE', 'Rg', 'Tair', 'VPD')) EProc$sMDSGapFill(i, FillAll = TRUE)
#'
#' # Export results and convert latent heat (LE) to evapotranspiration (ET)
#' # - typical ET units are mm hour-1 independent of actual measurement interval
#' results <- cbind(DETha98["timestamp"], EProc$sExportResults())
#' LE_vars <- c("LE_orig", "LE_f", "LE_fqc", "LE_fall", "LE_fsd")
#' ET_vars <- gsub("LE", "ET", LE_vars)
#' results[, ET_vars] <-
#'   lapply(LE_vars,
#'          function(x) LE.to.ET(results[, x], results$Tair_f) * 3600)
#' openeddy::units(results[ET_vars]) <- rep("mm hour-1", length(ET_vars))
#'
#' # Overwrite ET_fqc with proper values
#' results$ET_fqc <- results$LE_fqc
#' openeddy::units(results$ET_fqc) <- "-"
#'
#' # Aggregate uncertainty derived from look-up table standard deviation (SD)
#' # - sign and unit conversions are demonstrated
#' (unc <- agg_fsd(results, "%b-%y", agg_per = "month-1"))
#' lapply(unc, openeddy::units, names = TRUE)
#'
#' # Perform Lasslop et al. (2010) flux partitioning based on DayTime (DT) data
#' # - Reco and GPP uncertainty evaluation is available only for this method
#' # - Reichstein et al. (2005) Reco model uncertainty is not exported and
#' #   GPP is computed as residual (not modelled)
#' EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
#' EProc$sGLFluxPartition(suffix = "uStar")
#'
#' # Aggregate uncertainty derived from SD of Reco and GPP models
#' # - unit conversions are demonstrated
#' results <- cbind(DETha98["timestamp"], EProc$sExportResults())
#' (unc_DT <- agg_DT_SD(results, "%b-%y", agg_per = "month-1"))
#' lapply(unc_DT, openeddy::units, names = TRUE)
#' }
#' @export
agg_mean <- function(x, format, breaks = NULL, interval = NULL,
                     tz = "GMT", ...) {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!"timestamp" %in% x_names) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (any(is.na(x$timestamp))) stop("NAs in 'x$timestamp' not allowed")
  if (any(diff(as.numeric(x$timestamp)) !=
          mean(diff(as.numeric(x$timestamp))))) {
    stop("x$timestamp does not form regular sequence")
  }

  # automatic recognition of interval (allow manual setting?)
  # - must be on original timestamp
  range <- range(x$timestamp)
  if (is.null(interval)) {
    # automated estimation of interval
    interval <- median(diff(x$timestamp))
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- diff(seq(Sys.time(), by = interval, length.out = 2))
  }
  if (diff(range) < interval)
    stop("'interval' is larger than 'timestamp' range")
  # interval in fraction or multiple of 1 day
  d <- as.numeric(interval, units = "days")

  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  # How many records with given interval per day?
  # - must be computed on the grouped timestamp
  zz <- aggregate(x[, "timestamp"], list(Intervals = x$timestamp), length)
  zz$days <- zz$x*d # conversion to number of days per period (also fractional)
  zz$x <- NULL

  out <- aggregate(x[names(x) != "timestamp"],
                   list(Intervals = x$timestamp), mean, ...)
  out <- merge(zz, out, sort = FALSE)
  varnames(out) <- c("Intervals", "days", varnames(x[names(x) != "timestamp"]))
  units(out) <- c("-", "-", units(x[names(x) != "timestamp"]))
  names(out) <- c("Intervals", "days", paste0(names(out[-(1:2)]), "_mean"))
  return(out)
}

#' @rdname agg_mean
#' @export
agg_sum <- function(x, format, agg_per = NULL, breaks = NULL, interval = NULL,
                    NEE_scor = TRUE, GPP_scor = FALSE,
                    quant = grep("^PAR|^PPFD|^APAR", names(x), value = TRUE),
                    power = grep("^GR|^Rg|^SW|^SR|^LW|^LR|^Rn|^NETRAD|^H|^LE",
                                 names(x), value = TRUE),
                    carbon = grep("^NEE|^GPP|^Reco", names(x), value = TRUE),
                    ET = grep("^ET", names(x), value = TRUE),
                    tz = "GMT", ...) {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!"timestamp" %in% x_names) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (any(is.na(x$timestamp))) stop("NAs in 'x$timestamp' not allowed")
  if (any(diff(as.numeric(x$timestamp)) !=
          mean(diff(as.numeric(x$timestamp))))) {
    stop("x$timestamp does not form regular sequence")
  }

  # automatic recognition of interval (allow manual setting?)
  # - must be on original timestamp
  range <- range(x$timestamp)
  if (is.null(interval)) {
    # automated estimation of interval
    interval <- median(diff(x$timestamp))
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- diff(seq(Sys.time(), by = interval, length.out = 2))
  }
  if (diff(range) < interval)
    stop("'interval' is larger than 'timestamp' range")
  # interval in fraction or multiple of 1 day
  d <- as.numeric(interval, units = "days")
  # interval in seconds
  interval <- as.numeric(interval, units = "secs")

  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  # How many records with given interval per day?
  # - must be computed on the grouped timestamp
  zz <- aggregate(x[, "timestamp"], list(Intervals = x$timestamp), length)
  zz$days <- zz$x*d # conversion to number of days per period (also fractional)
  zz$x <- NULL

  # Change sign in all NEE variables
  NEE_cols <- names(x) %in% grep("NEE", names(x[carbon]), value = TRUE)
  NEE <- names(x)[NEE_cols]
  if (NEE_scor) x[NEE_cols] <- -x[NEE_cols]

  # Perform GPP sign correction
  GPP <- grep("GPP", x_names, value = TRUE)
  if (GPP_scor) x[GPP] <- -x[GPP]
  if (NEE_scor | GPP_scor) {
    cat("Sign correction (x -> -x):\n")
    cat(if (length(c(NEE, GPP)) > 0) paste(
      NEE, GPP, collapse = ", ") else "None", "\n\n")
  }

  cat("Unit conversion\n===============\n")
  # conversion from quantum to radiometric units and to energy units
  # - from umol+1s-1m-2 to MJ+1hh-1m-2 (hh = half-hour)
  quant <- quant[quant %in% names(x)]
  x[quant] <- x[quant] * interval * 1e-6 / 4.57 # 4.57 Thimijan and Heins (1983)
  energy_units <- "MJ m-2"
  if (length(quant) > 0) {
    cat("Quantum to energy (", units(x[quant])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(quant, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(x[quant]) <- rep(energy_units, ncol(x[quant]))

  # conversion from power to energy units
  # - from W m-2 to MJ+1hh-1m-2
  power <- power[power %in% names(x)]
  x[power] <- x[power] * interval * 1e-6
  if (length(power) > 0) {
    cat("Power to energy (", units(x[power])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(power, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(x[power]) <- rep(energy_units, ncol(x[power]))

  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(x)]
  x[carbon] <- x[carbon] * interval * 12e-6
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        units(x[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(x[carbon]) <- rep(carbon_units, ncol(x[carbon]))

  # conversion for evapotranspiration
  # - from mm hour-1 to mm interval-1
  ET <- ET[ET %in% names(x)]
  x[ET] <- x[ET] / 3600 * interval
  ET_units <- "mm"
  if (length(ET) > 0) {
    cat("Evapotranspiration (", units(x[ET])[1],
        " -> ", trimws(paste(ET_units, agg_per)), "):\n\n",
        paste(ET, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(x[ET]) <- rep(ET_units, ncol(x[ET]))

  if (sum(length(c(quant, power, carbon, ET))) == 0)
    cat("No variables available for conversion\n")

  # Rename relevant NEE variables to NEP
  names(x)[NEE_cols] <- gsub("NEE", "NEP", names(x)[NEE_cols])

  out <- aggregate(x[names(x) != "timestamp"],
                   list(Intervals = x$timestamp), sum, ...)
  out <- merge(zz, out, sort = FALSE)
  varnames(out) <- c("Intervals", "days", varnames(x[names(x) != "timestamp"]))
  units(out) <- c("-", "-", units(x[names(x) != "timestamp"]))
  if (!is.null(agg_per)) units(out)[-(1:2)] <-
    trimws(paste(units(out)[-(1:2)], agg_per))
  names(out) <- c("Intervals", "days", paste0(names(out[-(1:2)]), "_sum"))
  return(out)
}

#' @rdname agg_mean
#' @export
agg_fsd <- function(x, format, agg_per = NULL, breaks = NULL, interval = NULL,
                    quant = grep("^PAR|^PPFD|^APAR", names(x), value = TRUE),
                    power = grep("^GR|^Rg|^SW|^SR|^LW|^LR|^Rn|^NETRAD|^H|^LE",
                                 names(x), value = TRUE),
                    carbon = grep("^NEE", names(x), value = TRUE),
                    ET = grep("^ET", names(x), value = TRUE),
                    tz = "GMT") {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!"timestamp" %in% x_names) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (any(is.na(x$timestamp))) stop("NAs in 'x$timestamp' not allowed")
  if (any(diff(as.numeric(x$timestamp)) !=
          mean(diff(as.numeric(x$timestamp))))) {
    stop("x$timestamp does not form regular sequence")
  }

  # automatic recognition of interval (allow manual setting?)
  # - must be on original timestamp
  range <- range(x$timestamp)
  if (is.null(interval)) {
    # automated estimation of interval
    interval <- median(diff(x$timestamp))
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- diff(seq(Sys.time(), by = interval, length.out = 2))
  }
  if (diff(range) < interval)
    stop("'interval' is larger than 'timestamp' range")
  # interval in fraction or multiple of 1 day
  d <- as.numeric(interval, units = "days")
  # interval in seconds
  interval <- as.numeric(interval, units = "secs")

  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  # How many records with given interval per day?
  # - must be computed on the grouped timestamp
  zz <- aggregate(x[, "timestamp"], list(Intervals = x$timestamp), length)
  zz$days <- zz$x*d # conversion to number of days per period (also fractional)
  nTot <- zz$x # required later for computation of sum from mean
  zz$x <- NULL

  fall_names <- grep("_fall$", names(x), value = TRUE) # no fall for GPP & Reco
  fall <- x[fall_names]
  names(fall) <- gsub("_fall", "", names(fall))

  orig_names <- grep("_orig$", names(x), value = TRUE) # no orig for GPP & Reco
  orig <- x[orig_names]
  names(orig) <- gsub("_orig", "", names(orig))
  orig <- orig[order(match(names(orig), names(fall)))] # order columns
  if (!identical(names(fall), names(orig))) stop(
    "'x' columns with suffix '_orig' not fitting '_fall' columns"
  )

  if (ncol(fall) == 0) return(NULL)
  resid <- orig - fall
  autoCorr <- lapply(resid, lognorm::computeEffectiveAutoCorr)

  fqc_names <- grep("_fqc$", names(x), value = TRUE)
  fqc_names <- fqc_names[
    !fqc_names %in% grep("^Ustar|^GPP", fqc_names, value = TRUE)]
  # order fqc_names
  fqc_names <- fqc_names[order(match(
    gsub("_fqc", "", fqc_names), names(fall)))]
  if (!identical(gsub("_fqc", "", fqc_names), names(fall))) stop(
    "'x' columns with suffix '_fqc' not fitting '_fall' columns"
  )

  resid_l <- split(resid, x$timestamp)
  l <- list()
  for (i in seq_along(resid_l)) l[[i]] <-
    mapply(lognorm::computeEffectiveNumObs, res = resid_l[[i]],
           effAcf = autoCorr, MoreArgs = list(na.rm = TRUE))
  nEff <- as.data.frame(do.call(rbind, l))

  fsd_names <- grep("_fsd$", names(x), value = TRUE)
  fsd <- x[fsd_names]
  names(fsd) <- gsub("_fsd", "", names(fsd))
  fsd <- fsd[order(match(names(fsd), names(fall)))] # order columns
  fsd_names <- names(fsd) # save the ordered variant - used later
  if (!identical(fsd_names, names(fall))) stop(
    "'x' columns with suffix '_fsd' not fitting '_fall' columns"
  )

  # SD considered only for measured records (fqc == 0)
  for (i in seq_along(fsd)) fsd[which(x[, fqc_names[i]] != 0), i] <- NA

  agg_SD <- aggregate(fsd, by = list(Intervals = x$timestamp), function(x)
    if (all(is.na(x))) NA_real_ else mean(x^2, na.rm = TRUE), drop = FALSE)
  agg_SD <- merge(zz, agg_SD, sort = FALSE)
  varnames(agg_SD[c("Intervals", "days")]) <- c("Intervals", "days")
  units(agg_SD[c("Intervals", "days")]) <- c("-", "-")

  res_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_SD[-(1:2)], nEff = nEff, SIMPLIFY = FALSE))
  names(res_SD) <- paste0(fsd_names, "_fsd")
  varnames(res_SD) <- names(res_SD)
  units(res_SD) <- units(fsd)

  res_mean <- res_sum <- cbind(agg_SD[c("Intervals", "days")], res_SD)

  # Compute sums as mean * nTot
  res_sum[-(1:2)] <-
    as.data.frame(lapply(res_mean[-(1:2)], function(x) x * nTot))

  cat("Unit conversion\n===============\n")
  # conversion from quantum to radiometric units and to energy units
  # - from umol+1s-1m-2 to MJ+1hh-1m-2 (hh = half-hour)
  quant <- quant[quant %in% names(res_sum)]
  res_sum[quant] <- as.data.frame(lapply(res_sum[quant], function(x)
    x * interval * 1e-6 / 4.57)) # 4.57 Thimijan and Heins (1983)
  energy_units <- "MJ m-2"
  if (length(quant) > 0) {
    cat("Quantum to energy (", units(res_sum[quant])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(quant, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(res_sum[quant]) <- rep(energy_units, ncol(res_sum[quant]))

  # conversion from power to energy units
  # - from W m-2 to MJ+1hh-1m-2
  power <- power[power %in% names(res_sum)]
  res_sum[power] <- as.data.frame(lapply(res_sum[power], function(x)
    x * interval * 1e-6))
  if (length(power) > 0) {
    cat("Power to energy (", units(res_sum[power])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(power, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(res_sum[power]) <- rep(energy_units, ncol(res_sum[power]))

  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(res_sum)]
  res_sum[carbon] <- as.data.frame(lapply(res_sum[carbon], function(x)
    x * interval * 12e-6))
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        units(res_sum[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(res_sum[carbon]) <- rep(carbon_units, ncol(res_sum[carbon]))

  # conversion for evapotranspiration
  # - from mm hour-1 to mm interval-1
  ET <- ET[ET %in% names(res_sum)]
  res_sum[ET] <- as.data.frame(lapply(res_sum[ET], function(x)
    x / 3600 * interval))
  ET_units <- "mm"
  if (length(ET) > 0) {
    cat("Evapotranspiration (", units(res_sum[ET])[1],
        " -> ", trimws(paste(ET_units, agg_per)), "):\n\n",
        paste(ET, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(res_sum[ET]) <- rep(ET_units, ncol(res_sum[ET]))

  if (sum(length(c(quant, power, carbon, ET))) == 0)
    cat("No variables available for conversion\n")

  # Rename relevant NEE variables to NEP
  NEE_cols <-
    names(res_sum) %in% grep("NEE", names(res_sum[carbon]), value = TRUE)
  names(res_sum)[NEE_cols] <- gsub("NEE", "NEP", names(res_sum)[NEE_cols])

  names(res_mean)[-(1:2)] <- paste0(names(res_mean[-(1:2)]), "_mean")
  names(res_sum)[-(1:2)] <- paste0(names(res_sum[-(1:2)]), "_sum")
  if (!is.null(agg_per)) units(res_sum)[-(1:2)] <-
    trimws(paste(units(res_sum)[-(1:2)], agg_per))

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

#' @rdname agg_mean
#' @export
agg_DT_SD <- function(x, format, agg_per = NULL, breaks = NULL, interval = NULL,
                      carbon = grep("^Reco|^GPP", names(x), value = TRUE),
                      tz = "GMT") {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!"timestamp" %in% x_names) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (any(is.na(x$timestamp))) stop("NAs in 'x$timestamp' not allowed")
  if (any(diff(as.numeric(x$timestamp)) !=
          mean(diff(as.numeric(x$timestamp))))) {
    stop("x$timestamp does not form regular sequence")
  }

  # automatic recognition of interval (allow manual setting?)
  # - must be on original timestamp
  range <- range(x$timestamp)
  if (is.null(interval)) {
    # automated estimation of interval
    interval <- median(diff(x$timestamp))
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- diff(seq(Sys.time(), by = interval, length.out = 2))
  }
  if (diff(range) < interval)
    stop("'interval' is larger than 'timestamp' range")
  # interval in fraction or multiple of 1 day
  d <- as.numeric(interval, units = "days")
  # interval in seconds
  interval <- as.numeric(interval, units = "secs")

  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  # How many records with given interval per day?
  # - must be computed on the grouped timestamp
  zz <- aggregate(x[, "timestamp"], list(Intervals = x$timestamp), length)
  zz$days <- zz$x*d # conversion to number of days per period (also fractional)
  nTot <- zz$x # required later for computation of sum from mean
  zz$x <- NULL

  orig_names <- grep("^NEE_.*_orig$", names(x), value = TRUE)
  orig <- x[orig_names]
  names(orig) <- gsub("^NEE_|_orig$", "", names(orig))

  if (ncol(orig) == 0) return(NULL)

  # Order following columns according to orig
  DT_names <- grep("_DT_", names(x), value = TRUE)

  Reco_names <- grep("Reco", DT_names, value = TRUE)
  Reco_SD <- x[grep("_SD$", Reco_names, value = TRUE)]
  Reco <- x[Reco_names[!Reco_names %in% names(Reco_SD)]]
  names(Reco_SD) <- gsub("^Reco_DT_|_SD$", "", names(Reco_SD))
  names(Reco) <- gsub("^Reco_DT_", "", names(Reco_SD))
  # order Reco(_SD) columns
  Reco_SD <- Reco_SD[order(match(names(Reco_SD), names(orig)))]
  Reco <- Reco[order(match(names(Reco), names(orig)))]

  GPP_names <- grep("GPP", DT_names, value = TRUE)
  GPP_SD <- x[grep("_SD$", GPP_names, value = TRUE)]
  GPP <- x[GPP_names[!GPP_names %in% names(GPP_SD)]]
  names(GPP_SD) <- gsub("^GPP_DT_|_SD$", "", names(GPP_SD))
  names(GPP) <- gsub("^GPP_DT_", "", names(GPP_SD))
  # order GPP(_SD) columns
  GPP_SD <- GPP_SD[order(match(names(GPP_SD), names(orig)))]
  GPP <- GPP[order(match(names(GPP), names(orig)))]
  if (!all(sapply(list(names(Reco_SD), names(GPP), names(GPP_SD)),
                  identical, names(Reco)))) {
    stop("'x' columns '^GPP_DT_' and '^Reco_DT_' not fitting")
  }
  orig <- orig[names(Reco_SD)]
  if (!identical(names(Reco_SD), names(orig))) stop(
    "'x' columns '^NEE_.*_orig$' not fitting '^Reco_DT_.*_SD$' columns"
  )
  if (ncol(orig) == 0) return(NULL)
  resid_DT <- orig - (Reco - GPP)
  autoCorr <- lapply(resid_DT, lognorm::computeEffectiveAutoCorr)

  resid_l <- split(resid_DT, x$timestamp)
  l <- vector("list", length(resid_l))
  for (i in seq_along(resid_l)) l[[i]] <-
    mapply(lognorm::computeEffectiveNumObs, res = resid_l[[i]],
           effAcf = autoCorr, MoreArgs = list(na.rm = TRUE))
  nEff_DT <- as.data.frame(do.call(rbind, l))

  # SD considered only for measured NEE records (fqc == 0)
  fqc_names <- grep("^NEE_.*_fqc$", names(x), value = TRUE)
  # order fqc_names
  fqc_names <- fqc_names[order(match(
    gsub("^NEE_|_fqc", "", fqc_names), names(orig)))]
  if (!identical(gsub("^NEE_|_fqc", "", fqc_names), names(Reco_SD))) stop(
    "'x' columns '^NEE_.*_fqc$' not fitting '^Reco_DT_.*_SD$' columns"
  )
  # fqc == 0: measured data; fqc != 0 gap-filled (should be excluded)
  for (i in seq_along(Reco_SD)) Reco_SD[which(x[, fqc_names[i]] != 0), i] <- NA
  for (i in seq_along(GPP_SD)) GPP_SD[which(x[, fqc_names[i]] != 0), i] <- NA

  agg_Reco_SD <-
    aggregate(Reco_SD, by = list(Intervals = x$timestamp), function(x)
      if (all(is.na(x))) NA_real_ else mean(x^2, na.rm = TRUE), drop = FALSE)
  agg_Reco_SD <- merge(zz, agg_Reco_SD, sort = FALSE)
  varnames(agg_Reco_SD[c("Intervals", "days")]) <- c("Intervals", "days")
  units(agg_Reco_SD[c("Intervals", "days")]) <- c("-", "-")

  res_Reco_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_Reco_SD[-(1:2)], nEff = nEff_DT, SIMPLIFY = FALSE))
  names(res_Reco_SD) <- paste0("Reco_DT_", names(Reco_SD), "_SD")
  varnames(res_Reco_SD) <- names(res_Reco_SD)
  units(res_Reco_SD) <- units(Reco_SD)

  agg_GPP_SD <-
    aggregate(GPP_SD, by = list(Intervals = x$timestamp), function(x)
      if (all(is.na(x))) NA_real_ else mean(x^2, na.rm = TRUE), drop = FALSE)

  res_GPP_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_GPP_SD[-1], nEff = nEff_DT, SIMPLIFY = FALSE))
  names(res_GPP_SD) <- paste0("GPP_DT_", names(GPP_SD), "_SD")
  varnames(res_GPP_SD) <- names(res_GPP_SD)
  units(res_GPP_SD) <- units(GPP_SD)

  res_mean <- res_sum <- cbind(agg_Reco_SD[c("Intervals", "days")],
                               res_Reco_SD, res_GPP_SD)

  # Compute sums as mean * nTot
  res_sum[-(1:2)] <- as.data.frame(lapply(res_mean[-(1:2)],
                                          function(x) x * nTot))

  cat("Unit conversion\n===============\n")
  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(res_sum)]
  res_sum[carbon] <- as.data.frame(lapply(res_sum[carbon], function(x)
    x * interval * 12e-6))
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        units(res_sum[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  units(res_sum[carbon]) <- rep(carbon_units, ncol(res_sum[carbon]))
  if (length(carbon) == 0)
    cat("No variables available for conversion\n")

  names(res_mean)[-(1:2)] <- paste0(names(res_mean[-(1:2)]), "_mean")
  names(res_sum)[-(1:2)] <- paste0(names(res_sum[-(1:2)]), "_sum")
  if (!is.null(agg_per)) units(res_sum)[-(1:2)] <-
    trimws(paste(units(res_sum)[-(1:2)], agg_per))

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020
# clean data frame, assign timestamp, establish columnames for flux,
# wind direction and year, remove missing values
#' @keywords internal
clean_df <- function(df, TimestampCol, targetCol, QcCol, wdCol, nInt) {
  #align column names and declare year
  vars <- c(TimestampCol, targetCol, QcCol, wdCol)
  if (!all(vars %in% names(df)))
    stop("not all specified column names (",
         paste(sQuote(vars), collapse = ", "),
         ") present in 'df'")
  # timestamp is expected to represent the start or middle of averaging interval
  date <- if (inherits(df[[TimestampCol]], "POSIXt")) {
    format(df[[TimestampCol]], format = "%Y%m%d%H%M", tz = "GMT")
  } else as.character(df[[TimestampCol]])
  df[df == -9999] <- NA
  df$wd <- df[[wdCol]]
  df$Year <- as.numeric(substr(date, 1,4))
  df$time <- as.numeric(substr(date, 9,12))

  #add column that contains eight wind sectors
  nSec <- as.integer(nInt)
  if (nSec == 1) {
    df$eight_sec <- factor(NA, levels = "[0,360)")
    df$eight_sec[!is.na(df$wd)] <- "[0,360)"
  } else {
    secAngle <- 360/nSec
    df$eight_sec <- cut(df$wd, seq(secAngle/2, 360-secAngle/2, by = secAngle),
                        right = FALSE, dig.lab = 4)
    north <- paste0('[', 360-secAngle/2, ',', secAngle/2, ')')
    levels(df$eight_sec) <- c(levels(df$eight_sec), north)
    df$eight_sec[df$wd >= 360-secAngle/2 | df$wd < secAngle/2] <- north
  }

  #add column that contains eight time intervals
  nTInt <- as.integer(nInt)
  df$time_int <- cut(df$time, seq(0, 2400, length.out = nTInt + 1),
                     right = FALSE, dig.lab = 4)
  if (nTInt != 1 && mean(diff(table(df$time_int)), na.rm = TRUE) != 0)
    warning("time intervals have unbalanced records")

  ## select only observational data
  ##only select observational data based on QC flag = 0
  # originally included bug when wd NAs were ignored
  qc <- if (is.null(QcCol)) TRUE else df[, QcCol] == 0
  df <- df[!is.na(df$wd) & qc & !is.na(df[[targetCol]]), ]
  return(df)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020/
# function to calculate traditional budget
# must be applied to cleaned data frame
# conv_fac from umol/m2/s to grams/m2/s (period treated internally)
# interval in seconds
#' @keywords internal
calc_uncorrected <- function(df, year, targetCol, interval, conv_fac) {
  #subset data frame by year
  subsetted <- df[df$Year == year, ]
  #calculate traditional budget based on simple sum of all observations and convert units
  conv <- conv_fac * interval
  budget <- sum(subsetted[[targetCol]]) * conv
  return(budget)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020/
# function to calculate standardized budgets based on the average wind pattern
# for all observation years
# must be applied to cleaned data frame
# conv_fac from umol/m2/s to grams/m2/s (period treated internally)
#' @keywords internal
calc_standardized <- function(df, year, targetCol, interval, conv_fac) {
  no_years <- length(unique(df$Year))
  df_l <- split(df, df$eight_sec)

  #subset dataframe by year [i]
  subsetted <- df[df$Year==year, ]

  #subset dataframe of year [i] into dataframes for each wind sector
  dfsub_l <- split(subsetted, subsetted$eight_sec)

  #carbon budget adjustment as outlined in Griebel et al. 2016:
  #step 1: define the average wind pattern as standardized wind sector
  #contribution to total observations of all years
  frac <- lapply(df_l, function(x) nrow(x)/no_years)

  #step 2: calculate the mean carbon uptake for each sector and multiply
  #with the average contribution of each sector
  cor <- mapply(function(x, y) x * mean(y[, targetCol]), frac, dfsub_l,
                SIMPLIFY = FALSE)

  #step 3: integrate across all sectors
  conv <- conv_fac * interval
  new_budget_year <- sum(unlist(cor), na.rm = TRUE) * conv
  return(new_budget_year)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020/
# function to calculate space equitable budgets where every wind sector
# contributes equally
# must be applied to cleaned data frame
# conv_fac from umol/m2/s to grams/m2/s (period treated internally)
#' @keywords internal
calc_space_eq <- function(df, year, targetCol, interval, conv_fac, normalize,
                          nInt) {
  #subset dataframe by year [i]
  subsetted <- df[df$Year==year, ]

  #step 1: subset dataframe of year [i] into dataframes for each wind sector
  dfsub_l <- split(subsetted, subsetted$eight_sec)

  #step 2: calculate mean carbon uptake for each sector and each year
  # 0.125 = 1/8 (8 sectors)
  contrb <- 1 / nInt
  obs_day <- 86400 / interval # seconds in day / interval
  obs_year <- obs_day * 365
  spaceEq <- obs_day * 365 * contrb
  conv <- conv_fac * interval

  cor <- lapply(dfsub_l, function(x) spaceEq * mean(x[, targetCol]))

  equit_budget_year <- sum(unlist(cor), na.rm = TRUE) * conv

  #check if normalization to number of observations is true
  if (normalize==TRUE) {
    equit_budget_year <- equit_budget_year/obs_year*nrow(subsetted)
  }
  return(equit_budget_year)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020/
# function to calculate space-time equitable budgets where every wind sector
# contributes equally and sector contributions are made time-uniform
# must be applied to cleaned data frame
# conv_fac from umol/m2/s to grams/m2/s (period treated internally)
#' @keywords internal
calc_spti_eq <- function(df, year, targetCol, interval, conv_fac, normalize) {
  #subset data frame by year
  subsetted <- df[df$Year==year, ]
  # subset by sectors
  dfsub_sp <- split(subsetted, subsetted$eight_sec)
  # subset each sector by time intervals and return mean
  # returns a matrix with space-time averages
  dfsub_spti <- sapply(dfsub_sp,
                       function(x) sapply(split(x[, targetCol], x$time_int),
                                          mean, na.rm = TRUE))

  #average by time first (row average) and then by space (column average)
  # time_means <- colMeans(dfsub_spti, na.rm=TRUE)
  if (length(levels(df$eight_sec)) == 1) {
    space_time_means <- dfsub_spti
  } else {
    space_means <- rowMeans(dfsub_spti, na.rm=TRUE)
    space_time_means <- mean(space_means, na.rm=TRUE)
  }

  obs_day <- 86400 / interval # seconds in day / interval
  obs_year <- obs_day * 365
  conv <- conv_fac * interval

  spti_eq_budget_year <- space_time_means * conv * obs_year

  #check if normalization to number of observations is true
  if (normalize==TRUE){
    spti_eq_budget_year <- spti_eq_budget_year/obs_year*nrow(subsetted)
  }
  return(spti_eq_budget_year)
}

# function used for bootstrapping within calc_spti_boot()
# x = fluxes within each bin
# min_rec = smallest nRec across bins
#' @keywords internal
boot <- function(x, min_rec) {
  mean(sample(x, size = min_rec, replace = TRUE), na.rm = TRUE)
}

# Modified Griebel-GRL_2020
# https://github.com/AnneGriebel/Griebel-GRL_2020/
# function to calculate space-time equitable budgets where every wind sector
# contributes equally and sector contributions are made time-uniform
# uncertainty bounds estimated using bootstrapping within each bin
# size of sample according to smallest nRec across bins
# must be applied to cleaned data frame
# conv_fac from umol/m2/s to grams/m2/s (period treated internally)
#' @keywords internal
calc_spti_boot <- function(df, year, targetCol, interval, conv_fac,
                           samples, normalize) {
  #subset data frame by year
  subsetted <- df[df$Year==year, ]
  # subset by sectors
  dfsub_sp <- split(subsetted, subsetted$eight_sec)
  # subset each sector by time intervals and return mean
  # returns a matrix with space-time averages
  dfsub_spti <- sapply(dfsub_sp,
                       function(x) sapply(split(x[, targetCol], x$time_int),
                                          mean, na.rm = TRUE))

  # returns a matrix with number of records per bin
  dfsub_nRec <- sapply(dfsub_sp,
                       function(x) sapply(split(x[, targetCol], x$time_int),
                                          length))
  min_rec <- min(dfsub_nRec)
  message("year ", year,
          " - minimum number of records across spatio-temporal bins: ",
          min_rec)

  # resample according to samples
  dfsub_l <- vector("list", samples)
  for (i in seq_len(samples)) {
    dfsub_l[[i]] <- sapply(dfsub_sp,
                           function(x) sapply(split(x[, targetCol], x$time_int),
                                              boot, min_rec = min_rec))
  }

  #average by time first (row average) and then by space (column average)
  # time_means <- colMeans(dfsub_spti, na.rm=TRUE)
  if (length(levels(df$eight_sec)) == 1) {
    spti_mean <- dfsub_spti

    # performed per each list element
    spti_means_l <- dfsub_l
    spti_quant <- quantile(unlist(spti_means_l), probs = c(0.05, 0.5, 0.95))
  } else {
    space_means <- rowMeans(dfsub_spti, na.rm=TRUE)
    spti_mean <- mean(space_means, na.rm=TRUE)

    # performed per each list element
    space_means_l <- lapply(dfsub_l, rowMeans, na.rm=TRUE)
    spti_means_l <- lapply(space_means_l, mean, na.rm=TRUE)
    spti_quant <- quantile(unlist(spti_means_l), probs = c(0.05, 0.5, 0.95))
  }

  obs_day <- 86400 / interval # seconds in day / interval
  obs_year <- obs_day * 365
  conv <- conv_fac * interval

  spti_budgets_year <- c(spti_mean, spti_quant) * conv * obs_year

  #check if normalization to number of observations is true
  if (normalize==TRUE){
    spti_budgets_year <- spti_budgets_year/obs_year*nrow(subsetted)
  }
  return(spti_budgets_year)
}

#' Compute Griebel et al. (2020) budgets
#'
#' Yearly budgets with different consideration of space-time equity.
#'
#' The function produces several variants of budgets that represent annual sums
#' of measured and quality checked flux with different consideration of
#' space-time equity. In order to obtain budgets in sensible units after
#' summation, appropriate \code{flux} type must be specified. E.g. conversion
#' factor for carbon fluxes (umol(CO2) m-2 s-1 -> g(C) m-2 s-1) is 12.0107e-6,
#' conversion factor for energy fluxes (W m-2 -> MJ m-2 s-1) is 1e-6. Temporal
#' aspect of the conversion is handled based on \code{interval} extent.
#'
#' Available variants of budgets include Traditional budget (uncorrected sum of
#' measured fluxes), Standardized budget (corrected according to wind sector
#' climatology based on all observation years), Space-equitable budget (each
#' sector contributes the exact same amount to budget) and Space-time-equitable
#' budget (each sector contributes equally to budget and sector contributions
#' are made time-uniform). Computation is generalized for any number of
#' \code{nInt} and any extent of \code{interval}. Please notice that Traditional
#' budget and Standardized budget differ only if multiple years are used for
#' computation. The reliability of the results depends on the data availability
#' within each year. For details see \code{References}.
#'
#' Arguments specifying \code{df} column names represent FLUXNET standard. To
#' process \code{REddyProc} outputs, timestamp must be corrected to represent
#' middle of averaging period and appropriate columns selected (see
#' \code{Examples}).
#'
#' @section Sign Correction: Although common sign convention for measured NEE
#'   (Net Ecosystem Exchange) denotes negative fluxes as CO2 uptake, summed NEE
#'   is typically reported with the opposite sign convention and is assumed to
#'   converge to NEP (Net Ecosystem Production), especially over longer
#'   aggregation intervals. In case of GPP (Gross Primary Production),
#'   \code{REddyProc} package applies sign convention denoting positive fluxes
#'   as carbon sink, thus sign correction before summing is not needed.
#'
#'   Since there is no reliable way to guess the sign convention used in the
#'   data set, \code{NEE_scor} and \code{GPP_scor} must be specified. The
#'   default values (\code{NEE_scor = TRUE}; \code{GPP_scor = FALSE}) are
#'   adapted to sign convention applied in \code{REddyProc} package.
#'
#' @section References:  Griebel, A., Metzen, D., Pendall, E., Burba, G., &
#'   Metzger, S. (2020). Generating spatially robust carbon budgets from flux
#'   tower observations. Geophysical Research Letters, 47, e2019GL085942.
#'   https://doi.org/10.1029/2019GL085942
#'
#' @param df A data frame.
#' @param TimestampCol A character string. Specifies a column name in \code{df}
#'   that carries date-time information either in \code{POSIXt} or text strings
#'   of format \code{"\%Y\%m\%d\%H\%M"}. Date-time information is expected to
#'   represent either start or middle of the averaging period.
#' @param wdCol A character string. Specifies a column name in \code{df} that
#'   carries the wind direction in degrees.
#' @param targetCol A character string. Specifies a column name in \code{df}
#'   that carries the flux values for budget computations.
#' @param QcCol A character string or \code{NULL}. Specifies a column name in
#'   \code{df} that carries gap-filling quality flags of \code{targetCol}
#'   variable. It is assumed that \code{df[, QcCol] == 0} identifies the
#'   measured (not gap-filled) records of \code{targetCol} variable. If
#'   \code{NULL}, all non-missing values of \code{targetCol} are used for
#'   budgeting.
#' @param interval An integer value. Represents an extent of eddy covariance
#'   averaging period in seconds (e.g. 1800 for 30 mins, 3600 for 60 mins).
#' @param flux A character string. What type of flux does \code{targetCol}
#'   represent? Can be abbreviated.
#' @param nInt An integer value. A number of wind sectors and time intervals for
#'   binning.
#' @param year An integer vector. If \code{NULL}, budgets are produced for all
#'   years available in \code{df}. Otherwise only specified years are processed.
#' @param NEE_scor,GPP_scor A logical value. Should sign correction of NEE (GPP)
#'   be performed?
#' @param normalize A logical value. If \code{TRUE} (default), space and
#'   space-time equitable budgets are corrected for the missing number of
#'   records in a year.
#'
#' @return A data frame with columns corresponding to year, different types of
#'   budgets and number of observations used for budget computation each year.
#'   Each column has assigned attributes varnames and units.
#'
#' @seealso \code{\link{spti_boot}} and \code{\link{spti_coverage}}.
#'
#' @examples
#' \dontrun{
#' library(REddyProc)
#'
#' # convert timestamp
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
#' Day = 'DoY', Hour = 'Hour')[-(2:4)]
#'
#' # generate randomly wind directions for demonstration purposes (not included)
#' DETha98$WD <- sample(0:359, nrow(DETha98), replace = TRUE)
#'
#' # if QcCol = NULL, all non-missing values of targetCol are used for budgeting
#' not_filled <- DETha98
#' not_filled$DateTime <- not_filled$DateTime - 900
#' Griebel20_budgets(not_filled, "DateTime", "WD", "LE", NULL, flux = "energy")
#'
#' # gap-filling is not needed but illustrates processing of FLUXNET data
#' # notice that ustar filtering of NEE should be applied before budgeting
#' DETha98 <- filterLongRuns(DETha98, "NEE")
#' EProc <- sEddyProc$new('DE-Tha', DETha98,
#' c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#' EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
#' filled <- cbind(DETha98, EProc$sExportResults())
#'
#' # correct timestamp to represent middle of averaging period
#' filled$DateTime <- filled$DateTime - 900
#' Griebel20_budgets(filled, "DateTime", "WD", "NEE", "NEE_uStar_fqc")
#' }
#' @export
Griebel20_budgets <- function(df,
                              TimestampCol = "TIMESTAMP_START",
                              wdCol = 'WD',
                              targetCol = "NEE_VUT_REF",
                              QcCol = "NEE_VUT_REF_QC",
                              interval = 1800L,
                              flux = c("carbon", "energy"),
                              nInt = 8L,
                              year = NULL,
                              NEE_scor = TRUE,
                              GPP_scor = FALSE,
                              normalize = TRUE) {
  #clean data frame, assign timestamp, establish columnames for flux,
  # wind direction and year, remove missing values
  df <- clean_df(df, TimestampCol = TimestampCol, targetCol = targetCol,
                 QcCol = QcCol, wdCol= wdCol, nInt = nInt)
  flux <- match.arg(flux)
  if (flux == "carbon") {
    conv_fac <- 12.0107e-6
    units <- "g(C) m-2 year-1"
  } else {
    conv_fac <- 1e-6
    units <- "MJ m-2 year-1"
  }

  isNEE <- grepl("NEE", targetCol)
  if (isNEE) {
    # Rename NEE to NEP if present
    targetCol <- names(df)[names(df) == targetCol] <- gsub(
      "NEE", "NEP", targetCol)
    # Change sign in NEE variable
    if (NEE_scor) {
      df[targetCol] <- -df[targetCol]
      cat('Sign correction: df$', targetCol, ' -> -df$', targetCol, '\n',
          sep = '')
    }
  }

  isGPP <- grepl("GPP", targetCol)
  if (isGPP) {
    # Change sign in GPP variable
    if (GPP_scor) {
      df[targetCol] <- -df[targetCol]
      cat('Sign correction: df$', targetCol, ' -> -df$', targetCol, '\n',
          sep = '')
    }
  }

  #identify unique years
  years <- if (is.null(year)) unique(df$Year) else year

  # create empty DataFrame to store results
  results <- data.frame(matrix(ncol = 6, nrow = length(years)))
  names(results) <- c(
    'year',
    paste(targetCol,
          c('traditional_budget', 'standardized_budget',
            'space_equitable_budget', 'space_time_equitable_budget',
            'n_obs'), sep = "_"))

  # set initial row number
  i <- 1

  # loop through each year
  for (y in years){
    # calculate uncorrected and corrected budgets
    uncorr <- calc_uncorrected(df, year = y, targetCol = targetCol,
                               interval = interval, conv_fac = conv_fac)
    corr <- calc_standardized(df, year = y, targetCol = targetCol,
                              interval = interval, conv_fac = conv_fac)
    space_equit <- calc_space_eq(df, year = y, targetCol = targetCol,
                                 interval = interval, conv_fac = conv_fac,
                                 normalize = normalize, nInt = nInt)
    spti_equit <- calc_spti_eq(df, year = y, targetCol = targetCol,
                               interval = interval, conv_fac = conv_fac,
                               normalize = normalize)
    # calculate number of observations results are based on
    n_obs <- nrow(df[df$Year == y, ])

    # generate rows for site and year y
    annual_result <- c(y, uncorr, corr, space_equit, spti_equit, n_obs)
    results[i, ] <- annual_result

    # increase row number by 1
    i <- i + 1
  }
  varnames(results) <- names(results)
  units(results) <- c("-", rep(units, 4), "#")

  return(results)
}

#' Space-time-equitable budgets with bootstrapping
#'
#' Yearly space-time-equitable budgets with uncertainty estimation.
#'
#' Data from individual years are separated to \code{nInt} number of bins (e.g.
#' for \code{nInt = 8} that is \code{8x8 = 64} bins) as in the original Griebel
#' et al. (2020) method. Each bin is then resampled \code{samples} amount of
#' times with \code{\link{sample}} \code{size} according to the smallest amount
#' of records across all bins. In addition to space-time-equitable budget of
#' original dataset (space_time_eq_orig), 5\%, 50\% and 95\% quantiles of
#' resampled datasets (space_time_eq_q05, space_time_eq_q50, space_time_eq_q95)
#' are provided for uncertainty assessment.
#'
#' In order to obtain budgets in sensible units after summation, appropriate
#' \code{flux} type must be specified. E.g. conversion factor for carbon fluxes
#' (umol(CO2) m-2 s-1 -> g(C) m-2 s-1) is 12.0107e-6, conversion factor for
#' energy fluxes (W m-2 -> MJ m-2 s-1) is 1e-6. Temporal aspect of the
#' conversion is handled based on \code{interval} extent.
#'
#' Space-time-equitable budgeting assures that each sector contributes equally
#' to budget and sector contributions are made time-uniform. Computation is
#' generalized for any number of \code{nInt} and any extent of \code{interval}.
#' Please notice that the reliability of the results depends on the data
#' availability within each year. For details see \code{References}.
#'
#' Arguments specifying \code{df} column names represent FLUXNET standard. To
#' process \code{REddyProc} outputs, timestamp must be corrected to represent
#' middle of averaging period and appropriate columns selected (see
#' \code{Examples}).
#'
#' @section Sign Correction: Although common sign convention for measured NEE
#'   (Net Ecosystem Exchange) denotes negative fluxes as CO2 uptake, summed NEE
#'   is typically reported with the opposite sign convention and is assumed to
#'   converge to NEP (Net Ecosystem Production), especially over longer
#'   aggregation intervals. In case of GPP (Gross Primary Production),
#'   \code{REddyProc} package applies sign convention denoting positive fluxes
#'   as carbon sink, thus sign correction before summing is not needed.
#'
#'   Since there is no reliable way to guess the sign convention used in the
#'   data set, \code{NEE_scor} and \code{GPP_scor} must be specified. The
#'   default values (\code{NEE_scor = TRUE}; \code{GPP_scor = FALSE}) are
#'   adapted to sign convention applied in \code{REddyProc} package.
#'
#' @section References:  Griebel, A., Metzen, D., Pendall, E., Burba, G., &
#'   Metzger, S. (2020). Generating spatially robust carbon budgets from flux
#'   tower observations. Geophysical Research Letters, 47, e2019GL085942.
#'   https://doi.org/10.1029/2019GL085942
#'
#' @param df A data frame.
#' @param TimestampCol A character string. Specifies a column name in \code{df}
#'   that carries date-time information either in \code{POSIXt} or text strings
#'   of format \code{"\%Y\%m\%d\%H\%M"}. Date-time information is expected to
#'   represent either start or middle of the averaging period.
#' @param wdCol A character string. Specifies a column name in \code{df} that
#'   carries the wind direction in degrees.
#' @param targetCol A character string. Specifies a column name in \code{df}
#'   that carries the flux values for budget computations.
#' @param QcCol A character string or \code{NULL}. Specifies a column name in
#'   \code{df} that carries gap-filling quality flags of \code{targetCol}
#'   variable. It is assumed that \code{df[, QcCol] == 0} identifies the
#'   measured (not gap-filled) records of \code{targetCol} variable. If
#'   \code{NULL}, all non-missing values of \code{targetCol} are used for
#'   budgeting.
#' @param interval An integer value. Represents an extent of eddy covariance
#'   averaging period in seconds (e.g. 1800 for 30 mins, 3600 for 60 mins).
#' @param flux A character string. What type of flux does \code{targetCol}
#'   represent? Can be abbreviated.
#' @param nInt An integer value. A number of wind sectors and time intervals for
#'   binning.
#' @param year An integer vector. If \code{NULL}, budgets are produced for all
#'   years available in \code{df}. Otherwise only specified years are processed.
#' @param samples An integer value. Amount of bootstraps to produce.
#' @param NEE_scor,GPP_scor A logical value. Should sign correction of NEE (GPP)
#'   be performed?
#' @param normalize A logical value. If \code{TRUE} (default), space and
#'   space-time equitable budgets are corrected for the missing number of
#'   records in a year.
#'
#' @return A data frame with columns corresponding to year, space-time-equitable
#'   budget of original dataset (space_time_eq_orig), 5\%, 50\% and 95\%
#'   quantiles of resampled datasets (space_time_eq_q05, space_time_eq_q50,
#'   space_time_eq_q95) and number of observations used for budget computation
#'   each year.
#'
#' @seealso \code{\link{Griebel20_budgets}} and \code{\link{spti_coverage}}.
#'
#' @examples
#' \dontrun{
#' library(REddyProc)
#'
#' # convert timestamp
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
#' Day = 'DoY', Hour = 'Hour')[-(2:4)]
#'
#' # generate randomly wind directions for demonstration purposes (not included)
#' DETha98$WD <- sample(0:359, nrow(DETha98), replace = TRUE)
#'
#' # if QcCol = NULL, all non-missing values of targetCol are used for budgeting
#' not_filled <- DETha98
#' not_filled$DateTime <- not_filled$DateTime - 900
#' spti_boot(not_filled, "DateTime", "WD", "LE", NULL, flux = "energy")
#'
#' # gap-filling is not needed but illustrates processing of FLUXNET data
#' # notice that ustar filtering of NEE should be applied before budgeting
#' DETha98 <- filterLongRuns(DETha98, "NEE")
#' EProc <- sEddyProc$new('DE-Tha', DETha98,
#' c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#' EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
#' filled <- cbind(DETha98, EProc$sExportResults())
#'
#' # correct timestamp to represent middle of averaging period
#' filled$DateTime <- filled$DateTime - 900
#' spti_boot(filled, "DateTime", "WD", "NEE", "NEE_uStar_fqc")
#' }
#' @export
spti_boot <- function(df,
                      TimestampCol = "TIMESTAMP_START",
                      wdCol = 'WD',
                      targetCol = "NEE_VUT_REF",
                      QcCol = "NEE_VUT_REF_QC",
                      interval = 1800L,
                      flux = c("carbon", "energy"),
                      nInt = 8L,
                      year = NULL,
                      samples = 100L,
                      NEE_scor = TRUE,
                      GPP_scor = FALSE,
                      normalize = TRUE) {
  #clean data frame, assign timestamp, establish columnames for flux,
  # wind direction and year, remove missing values
  df <- clean_df(df, TimestampCol = TimestampCol, targetCol = targetCol,
                 QcCol = QcCol, wdCol= wdCol, nInt = nInt)

  flux <- match.arg(flux)
  if (flux == "carbon") {
    conv_fac <- 12.0107e-6
    units <- "g(C) m-2 year-1"
  } else {
    conv_fac <- 1e-6
    units <- "MJ m-2 year-1"
  }

  isNEE <- grepl("NEE", targetCol)
  if (isNEE) {
    # Rename NEE to NEP if present
    targetCol <- names(df)[names(df) == targetCol] <- gsub(
      "NEE", "NEP", targetCol)
    # Change sign in NEE variable
    if (NEE_scor) {
      df[targetCol] <- -df[targetCol]
      cat('Sign correction: df$', targetCol, ' -> -df$', targetCol, '\n',
          sep = '')
    }
  }

  isGPP <- grepl("GPP", targetCol)
  if (isGPP) {
    # Change sign in GPP variable
    if (GPP_scor) {
      df[targetCol] <- -df[targetCol]
      cat('Sign correction: df$', targetCol, ' -> -df$', targetCol, '\n',
          sep = '')
    }
  }

  #identify unique years
  years <- if (is.null(year)) unique(df$Year) else year

  # create empty DataFrame to store results
  results <- data.frame(matrix(ncol = 6, nrow = length(years)))
  names(results) <- c(
    'year',
    paste(targetCol,
          c('space_time_eq_orig', 'space_time_eq_q05',
            'space_time_eq_q50', 'space_time_eq_q95',
            'n_obs'), sep = "_"))

  # set initial row number
  i <- 1

  # loop through each year
  for (y in years){
    # calculate bootstrap results for space-time equitable budgets
    spti_equit <- calc_spti_boot(df, year = y, targetCol = targetCol,
                                 interval = interval, conv_fac = conv_fac,
                                 samples = samples, normalize = normalize)
    # calculate number of observations results are based on
    n_obs <- nrow(df[df$Year == y, ])

    # generate rows for site and year y
    annual_result <- c(y, spti_equit, n_obs)
    results[i, ] <- annual_result

    # increase row number by 1
    i <- i + 1
  }
  varnames(results) <- names(results)
  units(results) <- c("-", rep(units, 4), "#")

  return(results)
}

# function to calculate spatio-temporal sampling coverage
# must be applied to cleaned data frame
# year argument accepts value "all" (applied across all years)
#' @keywords internal
calc_spti_cov <- function(df, targetCol, year, nInt, plot) {
  if (nInt < 2) stop("spatio-temporal coverage relevant only for nInt > 1")
  #subset data frame by year
  if (year == "all") df$Year <- "all"
  subsetted <- df[df$Year==year, ]
  # subset by sectors
  dfsub_sp <- split(subsetted, subsetted$eight_sec)
  dfsub_len <- sapply(dfsub_sp,
                      function(x) sapply(split(x[, targetCol], x$time_int),
                                         length))
  dfsub_perc <- dfsub_len / nrow(subsetted)
  space_sums <- colSums(dfsub_perc, na.rm=TRUE)
  time_sums <- rowSums(dfsub_perc, na.rm=TRUE)
  space_cumul <- cumsum(c(0, sort(space_sums)))
  time_cumul <- cumsum(c(0, sort(time_sums)))

  uniform_cumul <- (0:nInt)/nInt
  SSC <- round(sum(space_cumul) / sum(uniform_cumul), 3)
  TSC <- round(sum(time_cumul) / sum(uniform_cumul), 3)
  STSC <- round(mean(c(SSC, TSC)), 3)

  if (plot == FALSE) {
    return(c(SSC = SSC, TSC = TSC, STSC = STSC))
  }

  x <- data.frame(uniform = uniform_cumul, SSC = space_cumul, TSC = time_cumul)
  sp <- ggplot2::ggplot(x, ggplot2::aes(x = uniform)) +
    ggplot2::geom_area(ggplot2::aes(y = uniform, fill = "grey60")) +
    ggplot2::geom_area(ggplot2::aes(y = SSC, fill = "grey90")) +
    ggplot2::geom_line(ggplot2::aes(y = SSC, color = "b")) +
    ggplot2::geom_point(ggplot2::aes(y = SSC, color = "b")) +
    ggplot2::scale_fill_identity(name = NULL,
                                 guide = 'legend',
                                 labels = c('uniform spatial coverage = 1',
                                            paste('spatial sampling coverage =',
                                                  SSC))) +
    ggplot2::scale_colour_manual(name = NULL, values =c('b'='black'),
                                 labels = c('observations')) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.1, 0.8),
                   legend.justification = "left",
                   legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = 'Spatial sampling coverage') +
    ggplot2::xlab('Expected cumulative contribution') +
    ggplot2::ylab('Observed cumulative contribution') +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                    col = ggplot2::guide_legend(order = 2))

  tp <- ggplot2::ggplot(x, ggplot2::aes(x = uniform)) +
    ggplot2::geom_area(ggplot2::aes(y = uniform, fill = "grey60")) +
    ggplot2::geom_area(ggplot2::aes(y = TSC, fill = "grey90")) +
    ggplot2::geom_line(ggplot2::aes(y = TSC, color = "b")) +
    ggplot2::geom_point(ggplot2::aes(y = TSC, color = "b")) +
    ggplot2::scale_fill_identity(name = NULL,
                                 guide = 'legend',
                                 labels = c('uniform temporal coverage = 1',
                                            paste('temporal sampling coverage =',
                                                  TSC))) +
    ggplot2::scale_colour_manual(name = NULL, values =c('b'='black'),
                                 labels = c('observations')) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = c(0.1, 0.8),
                   legend.justification = "left",
                   legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = 'Temporal sampling coverage') +
    ggplot2::xlab('Expected cumulative contribution') +
    ggplot2::ylab('Observed cumulative contribution') +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                    col = ggplot2::guide_legend(order = 2))

  list(spatial_sampling_coverage = sp, temporal_sampling_coverage = tp)
}

#' Calculate spatio-temporal sampling coverage
#'
#' Yearly estimates of spatial, temporal and spatio-temporal sampling coverage
#' based on comparison of ideal vs observed number of samples in spatial and/or
#' temporal bins.
#'
#' Arguments specifying \code{df} column names represent FLUXNET standard. To
#' process \code{REddyProc} outputs, timestamp must be corrected to represent
#' middle of averaging period and appropriate columns selected (see
#' \code{Examples}).
#'
#' @section References:  Griebel, A., Metzen, D., Pendall, E., Burba, G., &
#'   Metzger, S. (2020). Generating spatially robust carbon budgets from flux
#'   tower observations. Geophysical Research Letters, 47, e2019GL085942.
#'   https://doi.org/10.1029/2019GL085942
#'
#' @param df A data frame.
#' @param TimestampCol A character string. Specifies a column name in \code{df}
#'   that carries date-time information either in \code{POSIXt} or text strings
#'   of format \code{"\%Y\%m\%d\%H\%M"}. Date-time information is expected to
#'   represent either start or middle of the averaging period.
#' @param wdCol A character string. Specifies a column name in \code{df} that
#'   carries the wind direction in degrees.
#' @param targetCol A character string. Specifies a column name in \code{df}
#'   that carries the flux values for sampling coverage assessment.
#' @param QcCol A character string or \code{NULL}. Specifies a column name in
#'   \code{df} that carries gap-filling quality flags of \code{targetCol}
#'   variable. It is assumed that \code{df[, QcCol] == 0} identifies the
#'   measured (not gap-filled) records of \code{targetCol} variable. If
#'   \code{NULL}, all non-missing values of \code{targetCol} are used for
#'   budgeting.
#' @param plot A logical value. If \code{FALSE} (default), a data frame with
#'   sampling coverage values for each year is returned. Otherwise a list of
#'   \code{ggplot}s showing spatial and temporal sampling coverage for each year
#'   is returned.
#' @param nInt An integer value. A number of wind sectors and time intervals for
#'   binning.
#' @param year Either integer vector, character string \code{"all"} or
#'   \code{NULL}. If \code{NULL} (default), estimates are produced for each year
#'   available in \code{df}. If \code{"all"}, estimates are produced across all
#'   years. Otherwise only specified years are processed.
#'
#' @return If \code{plot = FALSE}, a data frame. If \code{plot = TRUE}, a named
#'   list of \code{ggplot} objects.
#'
#' @seealso \code{\link{Griebel20_budgets}} and \code{\link{spti_boot}}.
#'
#' @examples
#' \dontrun{
#' library(REddyProc)
#'
#' # convert timestamp
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
#' Day = 'DoY', Hour = 'Hour')[-(2:4)]
#'
#' # generate randomly wind directions for demonstration purposes (not included)
#' DETha98$WD <- sample(0:359, nrow(DETha98), replace = TRUE)
#'
#' # if QcCol = NULL, all non-missing values of targetCol are used for budgeting
#' not_filled <- DETha98
#' not_filled$DateTime <- not_filled$DateTime - 900
#'
#' spti_coverage(not_filled, "DateTime", "WD", "LE", NULL)
#' spti_coverage(not_filled, "DateTime", "WD", "LE", NULL, plot = TRUE)
#'
#' # gap-filling is not needed but illustrates processing of FLUXNET data
#' # notice that ustar filtering of NEE should be applied before budgeting
#' DETha98 <- filterLongRuns(DETha98, "NEE")
#' EProc <- sEddyProc$new('DE-Tha', DETha98,
#' c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#' EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
#' filled <- cbind(DETha98, EProc$sExportResults())
#'
#' # correct timestamp to represent middle of averaging period
#' filled$DateTime <- filled$DateTime - 900
#' spti_coverage(filled, "DateTime", "WD", "NEE", "NEE_uStar_fqc")
#' spti_coverage(filled, "DateTime", "WD", "NEE", "NEE_uStar_fqc", plot = TRUE)
#' }
#' @export
spti_coverage <- function(df,
                          TimestampCol = "TIMESTAMP_START",
                          wdCol = 'WD',
                          targetCol = "NEE_VUT_REF",
                          QcCol = "NEE_VUT_REF_QC",
                          plot = FALSE,
                          nInt = 8L,
                          year = NULL) {
  #clean data frame, assign timestamp, establish columnames for flux,
  # wind direction and year, remove missing values
  df <- clean_df(df, TimestampCol = TimestampCol, targetCol = targetCol,
                 QcCol = QcCol, wdCol= wdCol, nInt = nInt)
  #identify unique years
  years <- if (is.null(year)) unique(df$Year) else year

  # create empty DataFrame to store results
  if (plot == FALSE) {
    results <- data.frame(matrix(ncol = 4, nrow = length(years)))
    names(results) <- c('year', 'spatial_SC', 'temporal_SC',
                        'spatio_temporal_SC')
  } else {
    results <- vector('list', length = length(years))
    names(results) <- paste0('year_', years)
  }

  # set initial row number
  i <- 1

  # loop through each year
  for (y in years){
    # calculate sampling coverage
    spti_sc <- calc_spti_cov(df, targetCol = targetCol, year = y, nInt = nInt,
                             plot = plot)

    if (plot == FALSE) {
      # generate rows for site and year y
      annual_result <- c(y, spti_sc)
      results[i, ] <- annual_result
    } else {
      results[[i]] <- spti_sc
    }

    # increase row number by 1
    i <- i + 1
  }
  return(results)
}

