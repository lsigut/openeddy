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
#'   conversion can be applied to columns defined by \code{quant}, \code{power}
#'   and \code{carbon} arguments.
#'
#' @section Sign Correction: Although the sign convention used for measured NEE
#'   (Net Ecosystem Exchange) denotes negative fluxes as CO2 uptake, summed NEE
#'   is typically reported with the opposite sign convention and is assumed to
#'   converge to NEP (Net Ecosystem Production), especially over longer
#'   aggregation intervals. Similarly, estimated negative GPP (Gross Primary
#'   Production) typically denotes carbon sink but should be corrected to
#'   positive values if summed over a time period.
#'
#'   \code{agg_sum} automatically detects all NEE and GPP columns in \code{x}
#'   using regular expressions and changes their sign. For GPP columns, sign
#'   change is performed only if mean GPP < 0 (sign convention autodetection).
#'   Note that it assumes that average GPP signal denotes carbon sink and it
#'   could fail if such sink is missing or negligible (e.g. urban measurements).
#'   In cases when NEE or its uncertainty is summed (\code{agg_sum} or
#'   \code{agg_fsd}), NEE is renamed to NEP.
#'
#' @section References: Bayley, G. and Hammersley, J., 1946. The "Effective"
#'   Number of Independent Observations in an Autocorrelated Time Series.
#'   Supplement to the Journal of the Royal Statistical Society, 8(2), 184-197.
#'   doi: \url{https://doi.org/10.2307/2983560}
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
#' @param agg_per A character string providing the time interval of aggregation
#'   that will be appended to units (e.g. \code{"hh-1"}, \code{"week-1"} or
#'   \code{"month-1"}).
#' @param quant A character vector listing variable names that require
#'   conversion from quantum to energy units before aggregation.
#' @param power A character vector listing variable names that require
#'   conversion from power to energy units before aggregation.
#' @param carbon A character vector listing variable names that require
#'   conversion from CO2 concentration to C mass flux units before aggregation.
#' @param tz A character string specifying the time zone to be used for the
#'   conversion. System-specific (see \code{\link{as.POSIXlt}} or
#'   \code{\link{timezones}}), but \code{""} is the current time zone, and
#'   \code{"GMT"} is UTC. Invalid values are most commonly treated as UTC, on
#'   some platforms with a warning.
#' @param ... Further arguments to be passed to the internal
#'   \code{\link{aggregate}} function.
#'
#' @return \code{agg_mean} and \code{agg_sum} produce a data frame with
#'   attributes varnames and units assigned to each respective column.
#'
#'   \code{agg_fsd} and \code{agg_DT_SD} produce a list with two data frames
#'   \code{mean} and \code{sum} with attributes varnames and units assigned to
#'   each respective column or \code{NULL} value if required columns are not
#'   recognized.
#'
#' @seealso \code{\link{aggregate}}, \code{\link{as.POSIXlt}},
#'   \code{\link{cut.POSIXt}}, \code{\link{mean}}, \code{\link{regexp}},
#'   \code{\link{strftime}}, \code{\link{sum}}, \code{\link{timezones}},
#'   \code{\link{varnames}}
#'
#' @examples
#' \dontrun{
#' library(REddyProc)
#' DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
#' Day = 'DoY', Hour = 'Hour')[-(2:4)]
#' EProc <- sEddyProc$new('DE-Tha', DETha98,
#' c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#' names(DETha98)[1] <- "timestamp"
#' DETha98$timestamp <- DETha98$timestamp - 60*15
#' agg_mean(DETha98, "%m-%y")
#' agg_mean(DETha98, "%m-%y", na.rm = TRUE)
#' (zz <- agg_sum(DETha98, "%m-%y", agg_per = "month-1"))
#' units(zz, names = TRUE)
#'
#' EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
#' for (i in c('Tair', 'Rg', 'VPD')) EProc$sMDSGapFill(i, FillAll = TRUE)
#' results <- cbind(DETha98["timestamp"], EProc$sExportResults())
#' agg_fsd(results, "%m-%y", agg_per = "month-1")
#' EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
#' EProc$sGLFluxPartition(suffix = "uStar")
#' results <- cbind(DETha98["timestamp"], EProc$sExportResults())
#' agg_DT_SD(results, "%m-%y", agg_per = "month-1")}
agg_mean <- function(x, format, breaks = NULL, tz = "GMT", ...) {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!"timestamp" %in% x_names) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  out <- aggregate(x[names(x) != "timestamp"],
                   list(Intervals = x$timestamp), mean, ...)
  openeddy::varnames(out) <- c("Intervals",
                               openeddy::varnames(x[names(x) != "timestamp"]))
  openeddy::units(out) <- c("-", openeddy::units(x[names(x) != "timestamp"]))
  names(out) <- c("Intervals", paste0(names(out[-1]), "_mean"))
  return(out)
}

#' @rdname agg_mean
agg_sum <- function(x, format, agg_per = NULL, breaks = NULL,
                    quant = grep("^PAR|^PPFD|^APAR", names(x), value = TRUE),
                    power = grep("^GR|^Rg|^SW|^SR|^LW|^LR|^Rn|^NETRAD|^H|^LE",
                                 names(x), value = TRUE),
                    carbon = grep("^NEE|^GPP|^Reco", names(x), value = TRUE),
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
  tres <- max(diff(as.numeric(x$timestamp))) # Time resolution in secs
  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

  # Change sign in all NEE variables
  NEE_cols <- names(x) %in% grep("NEE", names(x[carbon]), value = TRUE)
  NEE <- names(x)[NEE_cols]
  x[NEE_cols] <- -x[NEE_cols]

  # Perform sign correction in case the mean GPP is negative
  GPP <- grep("GPP", x_names, value = TRUE)
  sign_cor <- if (length(GPP)) {
    sapply(x[GPP], function(x) mean(x, na.rm = TRUE) < 0)
  } else FALSE
  x[GPP][sign_cor] <- -x[GPP][sign_cor]
  cat("Sign correction (x -> -x):\n")
  cat(if (length(c(NEE, GPP[sign_cor])) > 0) paste(
    NEE, GPP[sign_cor], collapse = ", ") else "None", "\n\n")

  cat("Unit conversion\n===============\n")
  # conversion from quantum to radiometric units and to energy units
  # - from umol+1s-1m-2 to MJ+1hh-1m-2 (hh = half-hour)
  quant <- quant[quant %in% names(x)]
  x[quant] <- x[quant] * tres * 1e-6 / 4.57 # 4.57 Thimijan and Heins (1983)
  energy_units <- "MJ m-2"
  if (length(quant) > 0) {
    cat("Quantum to energy (", openeddy::units(x[quant])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(quant, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(x[quant]) <- rep(energy_units, ncol(x[quant]))

  # conversion from power to energy units
  # - from W m-2 to MJ+1hh-1m-2
  power <- power[power %in% names(x)]
  x[power] <- x[power] * tres * 1e-6
  if (length(power) > 0) {
    cat("Power to energy (", openeddy::units(x[power])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(power, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(x[power]) <- rep(energy_units, ncol(x[power]))

  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(x)]
  x[carbon] <- x[carbon] * tres * 12e-6
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        openeddy::units(x[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(x[carbon]) <- rep(carbon_units, ncol(x[carbon]))
  if (sum(length(c(quant, power, carbon))) == 0)
    cat("No variables available for conversion\n")

  # Rename relevant NEE variables to NEP
  names(x)[NEE_cols] <- gsub("NEE", "NEP", names(x)[NEE_cols])

  out <- aggregate(x[names(x) != "timestamp"],
                   list(Intervals = x$timestamp), sum, ...)
  openeddy::varnames(out) <- c("Intervals",
                               openeddy::varnames(x[names(x) != "timestamp"]))
  openeddy::units(out) <- c("-", openeddy::units(x[names(x) != "timestamp"]))
  if (!is.null(agg_per)) openeddy::units(out)[-1] <-
    trimws(paste(openeddy::units(out)[-1], agg_per))
  names(out) <- c("Intervals", paste0(names(out[-1]), "_sum"))
  return(out)
}

#' @rdname agg_mean
agg_fsd <- function(x, format, agg_per = NULL, breaks = NULL,
                    quant = grep("^PAR|^PPFD|^APAR", names(x), value = TRUE),
                    power = grep("^GR|^Rg|^SW|^SR|^LW|^LR|^Rn|^NETRAD|^H|^LE",
                                 names(x), value = TRUE),
                    carbon = grep("^NEE", names(x), value = TRUE),
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
  tres <- max(diff(as.numeric(x$timestamp))) # Time resolution in secs
  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

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
  openeddy::varnames(agg_SD["Intervals"]) <- "Intervals"
  openeddy::units(agg_SD["Intervals"]) <- "-"

  res_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_SD[-1], nEff = nEff, SIMPLIFY = FALSE))
  names(res_SD) <- paste0(fsd_names, "_fsd")
  openeddy::varnames(res_SD) <- names(res_SD)
  openeddy::units(res_SD) <- openeddy::units(fsd)

  res_mean <- res_sum <- cbind(agg_SD["Intervals"], res_SD)

  # Compute sums as mean * nTot
  nTot <- unname(sapply(resid_l, nrow))
  res_sum[-1] <- as.data.frame(lapply(res_mean[-1], function(x) x * nTot))

  cat("Unit conversion\n===============\n")
  # conversion from quantum to radiometric units and to energy units
  # - from umol+1s-1m-2 to MJ+1hh-1m-2 (hh = half-hour)
  quant <- quant[quant %in% names(res_sum)]
  res_sum[quant] <- as.data.frame(lapply(res_sum[quant], function(x)
    x * tres * 1e-6 / 4.57)) # 4.57 Thimijan and Heins (1983)
  energy_units <- "MJ m-2"
  if (length(quant) > 0) {
    cat("Quantum to energy (", openeddy::units(res_sum[quant])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(quant, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(res_sum[quant]) <- rep(energy_units, ncol(res_sum[quant]))

  # conversion from power to energy units
  # - from W m-2 to MJ+1hh-1m-2
  power <- power[power %in% names(res_sum)]
  res_sum[power] <- as.data.frame(lapply(res_sum[power], function(x)
    x * tres * 1e-6))
  if (length(power) > 0) {
    cat("Power to energy (", openeddy::units(res_sum[power])[1],
        " -> ", trimws(paste(energy_units, agg_per)), "):\n\n",
        paste(power, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(res_sum[power]) <- rep(energy_units, ncol(res_sum[power]))

  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(res_sum)]
  res_sum[carbon] <- as.data.frame(lapply(res_sum[carbon], function(x)
    x * tres * 12e-6))
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        openeddy::units(res_sum[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(res_sum[carbon]) <- rep(carbon_units, ncol(res_sum[carbon]))
  if (sum(length(c(quant, power, carbon))) == 0)
    cat("No variables available for conversion\n")

  # Rename relevant NEE variables to NEP
  NEE_cols <-
    names(res_sum) %in% grep("NEE", names(res_sum[carbon]), value = TRUE)
  names(res_sum)[NEE_cols] <- gsub("NEE", "NEP", names(res_sum)[NEE_cols])

  names(res_mean)[-1] <- paste0(names(res_mean[-1]), "_mean")
  names(res_sum)[-1] <- paste0(names(res_sum[-1]), "_sum")
  if (!is.null(agg_per)) openeddy::units(res_sum)[-1] <-
    trimws(paste(openeddy::units(res_sum)[-1], agg_per))

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

#' @rdname agg_mean
agg_DT_SD <- function(x, format, agg_per = NULL, breaks = NULL,
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
  tres <- max(diff(as.numeric(x$timestamp))) # Time resolution in secs
  if (!is.null(breaks)) {
    x$timestamp <- as.POSIXct(cut(x$timestamp, breaks = breaks), tz = tz)
  }
  x$timestamp <- strftime(x$timestamp, format = format, tz = tz)
  x$timestamp <- factor(x$timestamp, levels = unique(x$timestamp))

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
  l <- list()
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
  openeddy::varnames(agg_Reco_SD["Intervals"]) <- "Intervals"
  openeddy::units(agg_Reco_SD["Intervals"]) <- "-"

  res_Reco_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_Reco_SD[-1], nEff = nEff_DT, SIMPLIFY = FALSE))
  names(res_Reco_SD) <- paste0("Reco_DT_", names(Reco_SD), "_SD")
  openeddy::varnames(res_Reco_SD) <- names(res_Reco_SD)
  openeddy::units(res_Reco_SD) <- openeddy::units(Reco_SD)

  agg_GPP_SD <-
    aggregate(GPP_SD, by = list(Intervals = x$timestamp), function(x)
      if (all(is.na(x))) NA_real_ else mean(x^2, na.rm = TRUE), drop = FALSE)

  res_GPP_SD <- as.data.frame(mapply(function(SD, nEff)
    sqrt(SD / ifelse(nEff <= 1, NA_real_, nEff - 1)),
    SD = agg_GPP_SD[-1], nEff = nEff_DT, SIMPLIFY = FALSE))
  names(res_GPP_SD) <- paste0("GPP_DT_", names(GPP_SD), "_SD")
  openeddy::varnames(res_GPP_SD) <- names(res_GPP_SD)
  openeddy::units(res_GPP_SD) <- openeddy::units(GPP_SD)

  res_mean <- res_sum <- cbind(agg_GPP_SD["Intervals"], res_Reco_SD, res_GPP_SD)

  # Compute sums as mean * nTot
  nTot <- unname(sapply(resid_l, nrow))
  res_sum[-1] <- as.data.frame(lapply(res_mean[-1], function(x) x * nTot))

  cat("Unit conversion\n===============\n")
  # conversion from mean CO2 concentration flux to integrated mass flux of C
  # - from umol+1s-1m-2 to g(C)+1hh-1m-2
  carbon <- carbon[carbon %in% names(res_sum)]
  res_sum[carbon] <- as.data.frame(lapply(res_sum[carbon], function(x)
    x * tres * 12e-6))
  carbon_units <- "g(C) m-2"
  if (length(carbon) > 0) {
    cat("CO2 concentration to C mass flux (",
        openeddy::units(res_sum[carbon])[1],
        " -> ", trimws(paste(carbon_units, agg_per)), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(res_sum[carbon]) <- rep(carbon_units, ncol(res_sum[carbon]))
  if (length(carbon) == 0)
    cat("No variables available for conversion\n")

  names(res_mean)[-1] <- paste0(names(res_mean[-1]), "_mean")
  names(res_sum)[-1] <- paste0(names(res_sum[-1]), "_sum")
  if (!is.null(agg_per)) openeddy::units(res_sum)[-1] <-
    trimws(paste(openeddy::units(res_sum)[-1], agg_per))

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

