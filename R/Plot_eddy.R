#' Set Range for Plotting
#'
#' \code{setRange} makes sure that the range extracted from \code{x} will be
#' valid for plotting.
#'
#' Set range for a given numeric vector \code{x} subsetted by a logical vector
#' \code{filter}. If the subset contains any finite value, finite range of the
#' \code{x} subset is returned. Else if \code{x} contains any finite value,
#' finite range of \code{x} is returned. When no finite value can be found in
#' \code{x}, range is set manually (default \code{man = c(0, 0)}).
#'
#' If \code{x} length is higher than \code{filter} length, \code{filter} will be
#' recycled.
#'
#' @param x A numeric vector.
#' @param filter A logical vector that can be recycled if necessary to match the
#'   length of \code{x}.
#' @param man A numeric vector of length 2.
#'
#' @seealso \code{\link{range}}.
#'
#' @examples
#' \dontrun{
#' (aa <- c(1, NA, Inf, -Inf, 3, NaN, rep(NA, 4)))
#' range(aa, finite = TRUE)
#' setRange(aa, TRUE) # same effect
#' # Useful when applying filters
#' aa[rep(c(FALSE, TRUE), each = 5)]
#' suppressWarnings(range(aa[rep(c(FALSE, TRUE), each = 5)], finite = TRUE))
#' setRange(aa, rep(c(FALSE, TRUE), each = 5)) # range taken from unfiltered 'aa'
#' setRange(aa[c(FALSE, TRUE)]) # No finite values in 'x', applies 'man' range
#' }
#' @keywords internal
setRange <- function(x = NA, filter = TRUE, man = c(0, 0)) {
  if (!is.numeric(man) || length(man) != 2 || any(!is.finite(man))) {
    stop("'man' must be numeric vector with 2 finite values")
  }
  if (is.null(x) || !is.numeric(x) && !all(is.na(x))) {
    stop("'x' must be a vector containing numeric or NA values")
  }
  if (!is.logical(filter)) {
    stop("'filter' must be a logical vector")
  }
  if (length(x) %% length(filter) != 0) {
    warning(paste0("'x' length [", length(x), "] is not a multiple of 'filter'",
                  " length [", length(filter), "]"))
  }
  if (any(is.finite(x[filter]))) {
    return(range(x[filter], finite = TRUE))
  } else if (any(is.finite(x))) {
    return(range(x, finite = TRUE))
  } else {
    return(man)
  }
}

#' Plot Time Series of Eddy Covariance Data
#'
#' Visualize flux measurements together with micrometeorological or other
#' related variables in monthly and weekly intervals. Missing values, scales of
#' axes and plotting regions are treated in an automated way.
#'
#' The data frame \code{x} is expected to have certain properties. It is
#' required that it contains column named \code{"timestamp"} of class
#' \code{"POSIXt"} with regular sequence of date-time values, typically with
#' (half-)hourly time interval. Any missing values in \code{"timestamp"} are not
#' allowed. Thus, if no records exist for given date-time value, it still has to
#' be included. It also has to contain required (depends on the argument values
#' and applied modules) column names. If required auxiliary variable is not
#' available (e.g. \code{"Tair"}, \code{"PAR"}), it still has to be included in
#' \code{x} as a named column with \code{NA} values. The \code{x} column defined
#' by argument \code{flux} is the only one that has to contain at least one
#' non-missing value. If present, column \code{"P"} is expected to contain
#' precipitation.
#'
#' If \code{skip = "weekly"}, minimum requirements for \code{x} column names are
#' \code{"timestamp"} and \code{flux}. If \code{skip = "none"} or
#' \code{"monthly"}, respective names specified in 'Modules' (see below) are
#' also required.
#'
#' Variable names for plot labels are extracted from required column names of
#' \code{x}. Units are extracted from \code{x} if they are present as
#' \code{units} attributes of required columns. If missing, \code{"-"} string is
#' used instead.
#'
#' Plotting is separated into two stages. Firstly, \code{flux} time-series data
#' are drawn in monthly intervals. Monthly plotting regions consist of four
#' figures on top of each other representing separately four consecutive months.
#' Secondly, if \code{skip = "none"} or \code{"monthly"}, \code{flux}
#' time-series data are drawn together with auxiliary data in weekly intervals.
#' Weekly plotting regions are described in 'Modules' section (see below).
#'
#' @section Modules: Applies only if \code{skip = "none"} or \code{"monthly"}.
#'   Plotting of auxiliary variables in weekly intervals is simplified by using
#'   predefined modules. Their main purpose is to achieve well-arranged display
#'   of auxiliary variables. Weekly plotting regions consist of two figures
#'   representing separately two consecutive weeks. Each figure contains three
#'   panels on top of each other. The middle panel always contains the values
#'   from \code{flux} and, if present, \code{"P"} columns of \code{x}. Variables
#'   used in the upper and lower panel can be changed by \code{panel_top} and
#'   \code{panel_bottom}. These arguments specify the respective modules that
#'   will be loaded (can be the same) and thus also a certain set of required
#'   column names of \code{x} (variables).
#'
#'   Available modules are: \itemize{ \item T_light: requires \code{"Tair"},
#'   \code{"Tsoil"} and selected \code{light} columns. \item VPD_Rn: requires
#'   \code{"VPD"} and \code{"Rn"} columns. \item H_err_var: requires
#'   \code{"rand_err_H"} and \code{"ts_var"} columns. \item blue_red: requires
#'   columns specified by \code{panel_top/bottom_vars}. \item violet_orange:
#'   requires columns specified by \code{panel_top/bottom_vars}.}
#'
#'   Modules T_light, VPD_Rn and H_err_var have predefined color combinations
#'   and variables. Modules blue_red and violet_orange provide more flexibility
#'   as they only have predefined color combinations. Actual plotted variables
#'   are selected by additional argument (see above).
#'
#' @section Quality Control: \code{qc_flag} and \code{test} relate to QC flags
#'   available for the specified \code{flux}. Only QC scheme using QC flag range
#'   0 - 2 is supported.
#'
#'   Flags specified by \code{qc_flag} separate corresponding flux values to two
#'   groups. \emph{Used data} for flags 0 and 1 (black points) and
#'   \emph{Excluded data} for flags 2 and \code{NA} (grey points). The y-axis
#'   limits in the figures with \code{flux} values are based on \emph{Used data}
#'   only. If \code{qc_flag = "none"}, all data are \emph{Used data}. The time
#'   interval used for setting the range is one month both for monthly
#'   (respective month) and weekly (month centered on the respective week)
#'   plots. Additionally, maximum \code{ylim} range can be set for the figures
#'   with \code{flux} values (e.g. in case of large outliers).
#'
#'   In order to emphasize \code{flux} values with lower quality, \code{test}
#'   can be specified. Values with QC flag = 1 have green center of the point.
#'   Values with QC flag = 2 or \code{NA} have red center of the point.
#'
#'   NB: \code{flux} data with \code{NA} values are always \emph{Excluded data}
#'   and cannot be emphasized (\code{NA} values are not drawn).
#'
#' @section Gap-filling and NEE separation: Gap-filled flux values can be
#'   displayed using \code{flux_gf} as a line overlaying the \code{flux} values.
#'   If \code{NEE_sep = TRUE}, columns \code{"Reco"} and \code{"GPP"} are
#'   expected in \code{x}. \code{GPP_scor} allows to change the sign convention
#'   of GPP to minimize lines overlay during plotting. CO2 uptake in
#'   \code{REddyProc} package is represented by positive GPP, thus, to optimize
#'   plotting, \code{GPP_scor = TRUE} is taken as default.
#'
#' @section Abbreviations: \itemize{ \item H: Sensible heat flux [W m-2] \item
#'   NEE: Net Ecosystem Exchange [umol m-2 s-1] \item GPP: Gross Primary
#'   Production [umol m-2 s-1] \item Reco: Ecosystem Respiration [umol m-2 s-1]
#'   \item QC: Quality Control \item P: Precipitation [mm] \item PAR:
#'   Photosynthetic Active Radiation [umol m-2 s-1] \item GR: Global Radiation
#'   [W m-2] \item T: Temperature [degC] \item Tair: Air Temperature [degC]
#'   \item Tsoil: Soil Temperature [degC] \item VPD: Vapor Pressure Deficit
#'   [hPa] \item Rn: Net Radiation [W m-2] \item rand_err_H: random error of H
#'   [W m-2]; in plots abbreviated as H_re \item ts_var: sonic temperature
#'   variance [K2]}
#'
#' @param x A data frame with column names representing required variables. See
#'   'Details' below.
#' @param flux A character string. Specifies the column name in \code{x} with
#'   flux values.
#' @param qc_flag A character string. Specifies the column name in \code{x} with
#'   flux related quality control flag used for visualisation of data quality
#'   and setting of y-axis range. If "none" is provided, all data will be used.
#'   See 'Quality Control'.
#' @param test A character string. Specifies the column name in \code{x} with
#'   quality control flag for visualisation of its effect on the data. If "none"
#'   is provided, no visualisation will be done. See 'Quality Control'.
#' @param flux_gf A character string. Specifies the column name in \code{x} with
#'   gap-filled flux values.
#' @param NEE_sep A logical value. Determines whether NEE separation should be
#'   visualized. If \code{TRUE}, columns \code{"Reco"} and \code{"GPP"} are
#'   expected in \code{x}.
#' @param skip A character string. Determines whether plotting should be done in
#'   monthly (\code{skip = "weekly"}), weekly intervals (\code{skip =
#'   "monthly"}) or both (\code{skip = "none"}).
#' @param ylim A numeric vector of length two or \code{NULL}. If \code{NULL}
#'   (default), the treatment of y-axis limits in the figures with \code{flux}
#'   values is fully automated. Otherwise, \code{ylim} sets the maximum range of
#'   automatically computed y-axis limits. See 'Quality Control'.
#' @param panel_top A character string. Selects one of the available modules for
#'   plotting additional variables. This module is displayed above the panel
#'   with fluxes in weekly plots. Can be abbreviated. See 'Modules'.
#' @param panel_top_vars A character vector of length two or \code{NULL}.
#'   Specifies two variables expected in \code{x} if \code{panel_top =
#'   "blue_red"} or \code{"violet_orange"}, otherwise \code{NULL}.
#' @param panel_bottom A character string. Selects one of the available modules
#'   for plotting additional variables. This module is displayed below the panel
#'   with fluxes in weekly plots. Can be abbreviated. See 'Modules'.
#' @param panel_bottom_vars A character vector of length two or \code{NULL}.
#'   Specifies two variables expected in \code{x} if \code{panel_bottom =
#'   "blue_red"} or \code{"violet_orange"}, otherwise \code{NULL}.
#' @param light A character string. Required only for the \code{"T_light"}
#'   module. Selects preferred variable for incoming light intensity.
#'   \code{"PAR"} or \code{"GR"} is allowed. Can be abbreviated.
#' @param GPP_scor A logical value. Should sign correction of GPP be performed?
#'   See Gap-filling and NEE separation section in Details. Ignored if
#'   \code{NEE_sep = FALSE}.
#' @param document A logical value. If \code{TRUE}, values of \code{qc_flag} and
#'   \code{test} arguments are documented in both monthly and weekly plots.
#'
#' @seealso \code{\link{read_eddy}} and \code{\link{strptime_eddy}}.
#'
#' @examples
#' # prepare mock data
#' set.seed(87)
#' my_var <- sin(seq(pi / 2, 2.5 * pi, length = 48)) * 10
#' my_var[my_var > 5] <- 5
#' t <- seq(ISOdate(2020, 7, 1, 0, 15), ISOdate(2020, 7, 14, 23, 45), "30 mins")
#' P <- vector("numeric", 48 * 14)
#' P[c(180:188, 250:253, 360:366, 500:505)] <- sample(1:15, 26, replace = TRUE)
#' PAR <- (-my_var + 5) * 100
#' Tair <- Tsoil <- rep(-cos(seq(0, 2 * pi, length = 48)), 14)
#' Tair <- Tair * 2 + 15 + seq(0, 5, length = 48 * 14)
#' Tsoil <- Tsoil * 1.2 + 10 + seq(0, 3, length = 48 * 14)
#' VPD <- -my_var + 10
#' VPD <- VPD[c(43:48, 0:42)]
#' Rn <- PAR / 2 - 50
#'
#' # combine into data frame
#' a <- data.frame(
#'   timestamp = t,
#'   my_var = my_var + rnorm(48 * 14),
#'   my_qc = sample(c(0:2, NA), 672, replace = TRUE, prob = c(5, 3, 2, 1)),
#'   P = P,
#'   PAR = PAR,
#'   Tair = Tair,
#'   Tsoil = Tsoil,
#'   VPD = VPD,
#'   Rn = Rn
#' )
#'
#' # specify units
#' openeddy::units(a) <- c("-", "units", "-", "mm", "umol m-2 s-1", "degC",
#'                         "degC", "hPa", "W m-2")
#'
#' # plot in weekly resolution (flux can be any variable)
#' plot_eddy(x = a, flux = "my_var", qc_flag = "my_qc", test = "my_qc",
#' skip = "monthly")
#'
#' # test can be used to distinguish up to 3 groups (0-2 flagging scheme)
#' # - example with 2 groups:
#' a$day <- a$PAR > 0 # 2 groups (TRUE / FALSE; i.e. 1 / 0)
#' plot_eddy(a, "my_var", "my_qc", "day", skip = "monthly") # daytime is green
#' # - example with 3 groups:
#' a$Tair_levels <- cut(a$Tair, c(13, 16, 19, 22))
#' a$Tair_levels <- as.numeric(a$Tair_levels) - 1 # only flags 0-2 supported
#' plot_eddy(a, "my_var", "my_qc", "Tair_levels", skip = "monthly")
#'
#' # make a custom setup of top and bottom panels
#' plot_eddy(x = a, flux = "my_var", qc_flag = "my_qc", test = "my_qc",
#'           skip = "monthly",
#'           panel_top = "blue_red", panel_top_vars = c("Tair", "Rn"),
#'           panel_bottom = "violet_orange", panel_bottom_vars = c("PAR", "VPD"))
#'
#' # any time resolution is supported
#' b <- ex(a, c(TRUE, FALSE, FALSE, FALSE)) # two-hourly time resolution
#' plot_eddy(b, "my_var", "my_qc", "my_qc", skip = "monthly")
#'
#' # Precipitation is treated specifically and can be missing
#' d <- a
#' d["P"] <- NULL
#' plot_eddy(d, "my_var", "my_qc", "my_qc", skip = "monthly")
#'
#' @importFrom graphics lines points par grid axis.POSIXct axis abline mtext
#'   legend layout barplot
#' @export
plot_eddy <- function(x, flux, qc_flag = "none", test = "none",
                      flux_gf = "none", NEE_sep = FALSE,
                      skip = c("none", "monthly", "weekly"),
                      ylim = NULL,
                      panel_top = c("T_light", "VPD_Rn", "H_err_var",
                                    "blue_red", "violet_orange"),
                      panel_top_vars = NULL,
                      panel_bottom = c("VPD_Rn", "T_light", "H_err_var",
                                       "blue_red", "violet_orange"),
                      panel_bottom_vars = NULL,
                      light = c("PAR", "GR"),
                      GPP_scor = TRUE,
                      document = TRUE) {
  x_names <- names(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (any(!sapply(list(flux, qc_flag, test), is.character))) {
    stop("'flux', 'qc_flag', 'test' must be of class character")
  }
  req_vars <- c("timestamp", flux)
  req <- !c(qc_flag, test, flux_gf) %in% "none"
  req_vars <- c(req_vars, c(qc_flag, test, flux_gf)[req])
  if (NEE_sep) req_vars <- c(req_vars, "Reco", "GPP")
  skip <- match.arg(skip)
  if (skip != "weekly") {
    # precipitation is not a required variable anymore
    panel_top <- match.arg(panel_top)
    panel_bottom <- match.arg(panel_bottom)
    if ("T_light" %in% c(panel_top, panel_bottom)) {
      light <- match.arg(light)
      req_vars <- c(req_vars, light, "Tair", "Tsoil")
    }
    if ("VPD_Rn" %in% c(panel_top, panel_bottom)) {
      req_vars <- c(req_vars, "VPD", "Rn")
    }
    if ("H_err_var" %in% c(panel_top, panel_bottom)) {
      req_vars <- c(req_vars, "rand_err_H", "ts_var")
    }
    filter <- c(panel_top, panel_bottom) %in% c("blue_red", "violet_orange")
    if (any(filter)) {
      if (!all(sapply(
        list(panel_top_vars, panel_bottom_vars), is.character)[filter]) ||
        !all((sapply(
          list(panel_top_vars, panel_bottom_vars), length) == 2)[filter])) {
        stop("'panel_top/bottom_vars' must be a character vector of length 2")
      } else req_vars <- c(req_vars, panel_top_vars, panel_bottom_vars)
    }
  }
  if (!all(req_vars %in% x_names)) {
    stop(paste("missing", paste0(req_vars[!(req_vars %in% x_names)],
                                 collapse = ", ")))
  }
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  num_vars <- req_vars[!req_vars %in% c("timestamp", qc_flag, test)]
  check1 <- function(x) {!is.numeric(x) && !all(is.na(x))}
  res_c1 <- sapply(x[, num_vars], check1)
  if (any(res_c1)) {
    stop(paste("columns [", paste0(num_vars[res_c1], collapse = ", "),
               "] in 'x' must be of class numeric or contain NAs only",
               sep = ""))
  }
  if (qc_flag != "none" || test != "none") {
    check2 <- function(x) {!is.numeric(x) && !is.logical(x)}
    res_c2 <- sapply(x[, c(qc_flag, test)[c(qc_flag, test) != "none"]], check2)
    if (any(res_c2)) {
      stop(paste0("columns [",
                 paste0(c(qc_flag, test)[res_c2], collapse = ", "),
                 "] in 'x' must be of class numeric or logical"))
    }
  }
  time <- as.POSIXlt(x$timestamp)
  if (any(is.na(time))) stop("NAs in 'timestamp' not allowed")
  if (any(diff(as.numeric(time)) != mean(diff(as.numeric(time))))) {
    stop("timestamp does not form regular sequence")
  }
  units <- units(x, names = TRUE)
  wrap <- function(x) paste0("[", x, "]")
  date <- as.Date(x$timestamp)
  vals <- x[, flux]
  qc <- if (qc_flag == "none") 0L else x[, qc_flag] # qc_flag 0: show all data
  qc[is.na(qc)] <- 2L # NA qc is interpreted as flag 2
  exalt <- if (test == "none") 0L else x[, test] # exalt = 0: exalt not used
  exalt[is.na(exalt)] <- 2L # NA qc is interpreted as flag 2
  if (flux_gf != "none") vals_gf <- x[, flux_gf]
  if (NEE_sep) {
    Reco <- x[, "Reco"]
    GPP <- x[, "GPP"]
    if (GPP_scor) GPP <- -GPP # correct sign if needed
  }
  use <- qc < 2 & !is.na(vals)
  if (!any(use)) {
    stop("no non-missing values with accepted quality in 'flux'")
  }
  # Correcting $year (counting since 1900) and $mon (0-11 range)
  # Selects first day of month in the dataset
  day1 <- as.Date(paste(time$year[1] + 1900, "-", time$mon[1] + 1, "-01",
                        sep = ""))
  # Graphical display of data in monthly periods ===============================
  if (skip != "monthly") {
    # grey = excluded data, green = used data
    # Number of intervals with right side closure (+1)
    nInt <- length(unique(paste(time$year, time$mon))) + 1L
    # Create monthly intervals for plotting
    int <- seq(from = day1, length.out = nInt, by = "1 month")
    op <- par(mfcol = c(4, 1), mar = c(3, 0, 0, 0), oma = c(2, 6, 1, 1))
    for (i in 1:(nInt - 1L)) {
      mon <- date >= int[i] & date < int[i + 1L]
      # keep the lenght of time[mon] but remove excluded data
      # (needed for lines)
      showVals <- vals[mon]
      showVals[!use[mon]] <- NA
      # xaxis has to be one day longer to simplify plotting
      xaxis <- seq(from = int[i], to = int[i + 1L], by = "1 day")
      xaxis <- as.POSIXct(strptime(xaxis, "%Y-%m-%d", tz = "GMT"))
      y <- vals[use]
      f <- mon[use]
      if (flux_gf != "none") {
        y <- c(y, vals_gf)
        f <- c(f, mon)
      }
      if (NEE_sep) {
        y <- c(y, Reco, GPP)
        f <- c(f, rep(mon, 2))
      }
      y_range <- setRange(y, f)
      plot(time[mon], vals[mon], type = "n", xaxt = "n", yaxt = "n",
           ylab = "", xlab = "", panel.first = grid(nx = NA, ny = NULL),
           xlim = range(xaxis),
           ylim = c(max(ylim[1], y_range[1], na.rm = TRUE),
                    min(ylim[2], y_range[2], na.rm = TRUE)))
      # Halfday shift of ticks to mark the middle of day
      axis.POSIXct(1,
                   at = seq(min(xaxis), max(xaxis), by = "5 days") + 12 * 3600,
                   format = "%Y/%m/%d", padj = -0.5)
      axis(2)
      abline(v = xaxis, col = "grey", lty = 3)
      lines(time[mon], vals[mon], type = "o", pch = 19, cex = 0.75,
            col = "grey")
      lines(time[mon], showVals, type = "o", pch = 19, cex = 0.75)
      if (flux_gf != "none") {
        lines(time[mon], vals_gf[mon],
              type = "l", pch = 19, cex = 0.4, col = "forestgreen")
      }
      if (NEE_sep) {
        lines(time[mon], Reco[mon], col = "firebrick3")
        lines(time[mon], GPP[mon], col = "dodgerblue3")
      }
      points(time[mon & (exalt == 1)], vals[mon & (exalt == 1)],
             col = "green", pch = 19, cex = 0.4)
      points(time[mon & (exalt == 2)], vals[mon & (exalt == 2)],
             col = "red", pch = 19, cex = 0.4)
      mtext(wrap(units[flux]), 2, line = 2.2, cex = 0.8)
      mtext(flux, 2, line = 3.4, cex = 1.2)
      if (i %% 4 == 1) {
        legend("topleft",
               legend = c("Used data", "Excluded data", "Test flag = 1",
                          "Test flag = 2"),
               col = c("black", "grey", "green", "red"),
               pch = 19, lty = c(1, 1, NA, NA), bty = "n")
        if (flux_gf != "none" || NEE_sep) {
          legend("topright",
                 legend = c("Gap-filled data", if (NEE_sep) c("Reco", "GPP")),
                 col = c("forestgreen",
                         if (NEE_sep) c("firebrick3", "dodgerblue3")),
                 lwd = 2, bty = "n")
        }
      }
      if (document && i %% 4 == 3) {
        mtext(paste("qc_flag = '", qc_flag, "', test = '", test, "'",
                    sep = ""), side = 3, line = 0, adj = 0, cex = 0.6)
      }
      if (i %% 4 == 0) mtext("Day of year", 1, line = 3, cex = 1.2)
    }
    par(op)
  }
  if (skip != "weekly") {
    # is.null(x$P) does not work because of x$PAR
    if (sum(grepl("^P$", names(x)))) P <- x[, "P"] else P <- NULL
    # Modules ==================================================================
    modules <- list()
    if ("T_light" %in% c(panel_top, panel_bottom)) {
      PAR <- x[, light]
      Tair <- x$Tair
      Tsoil <- x$Tsoil
      modules$T_light <- function() {
        plot(time[week], Tair[week], xlim = range(xaxis),
             ylim = setRange(c(Tair, Tsoil), mon), type = "n",
             panel.first = grid(nx = NA, ny = NULL), xaxt = "n", yaxt = "n",
             ylab = "", xlab = "")
        abline(v = xaxis, col = "grey", lty = 3)
        axis(2, line = 1.8, padj = 0.9, tcl = -0.3)
        lines(time[week], Tair[week], lwd = 2, col = "dodgerblue")
        lines(time[week], Tsoil[week], lwd = 2, col = "red1")
        par(new = TRUE)
        plot(time[week], PAR[week], xlim = range(xaxis),
             ylim = setRange(PAR, mon), type = "l", col = "gold", lwd = 2,
             xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        axis(4, padj = -0.9, tcl = -0.3)
        mtext(paste("T", wrap(units["Tair"])), 2, line = 3.6)
        mtext(light, 4, line = 2)
        mtext(wrap(units[light]), 4, line = 3.6, cex = 0.8)
        if (i %% 2 == 1) {
          legend("topleft", legend = c("Tair", "Tsoil", light), lty = 1, lwd = 2,
                 col = c("dodgerblue", "red1", "gold"), bty = "n")
        }
      }
    }
    if ("VPD_Rn" %in% c(panel_top, panel_bottom)) {
      VPD <- x$VPD
      Rn <- x$Rn
      modules$VPD_Rn <- function() {
        plot(time[week], VPD[week], xlim = range(xaxis),
             ylim = setRange(VPD, mon), panel.first = grid(nx = NA, ny = NULL),
             type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
        abline(v = xaxis, col = "grey", lty = 3)
        axis(2, line = 1.8, padj = 0.9, tcl = -0.3)
        lines(time[week], VPD[week], lwd = 2, col = "mediumorchid")
        par(new = TRUE)
        plot(time[week], Rn[week], xlim = range(xaxis), ylim = setRange(Rn, mon),
             type = "l", col = "chocolate1", lwd = 2,
             xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        axis(4, padj = -0.9, tcl = -0.3)
        mtext(paste("VPD", wrap(units["VPD"])), 2, line = 3.6)
        mtext("Rn", 4, line = 2)
        mtext(wrap(units["Rn"]), 4, line = 3.6, cex = 0.8)
        if (i %% 2 == 1) {
          legend("topleft", legend = c("VPD", "Rn"), lty = 1, lwd = 2,
                 col = c("mediumorchid", "chocolate1"), bty = "n")
        }
      }
    }
    if ("H_err_var" %in% c(panel_top, panel_bottom)) {
      H_re <- x$rand_err_H
      ts_var <- x$ts_var
      modules$H_err_var <- function() {
        plot(time[week], H_re[week], xlim = range(xaxis),
             ylim = setRange(H_re, mon), panel.first = grid(nx = NA, ny = NULL),
             type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
        abline(v = xaxis, col = "grey", lty = 3)
        axis(2, line = 1.8, padj = 0.9, tcl = -0.3)
        lines(time[week], H_re[week], lwd = 2, col = "mediumorchid")
        par(new = TRUE)
        plot(time[week], ts_var[week],
             xlim = range(xaxis), ylim = setRange(ts_var, mon),
             type = "l", col = "chocolate1", lwd = 2,
             xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        axis(4, padj = -0.9, tcl = -0.3)
        mtext(paste("H_re", wrap(units["rand_err_H"])), 2, line = 3.6)
        mtext("ts_var", 4, line = 2)
        mtext(wrap(units["ts_var"]), 4, line = 3.6, cex = 0.8)
        if (i %% 2 == 1) {
          legend("topleft", legend = c("H_re", "ts_var"), lty = 1, lwd = 2,
                 col = c("mediumorchid", "chocolate1"), bty = "n")
        }
      }
    }
    if (any(filter)) { # a, b are variable names, col_comb is color combination
      modules$two_vars <- function(a, b, col_comb) {
        var1 <- x[, a]
        var2 <- x[, b]
        col_comb <- if (col_comb == "blue_red") c("dodgerblue", "red1") else
          c("mediumorchid", "chocolate1")
        plot(time[week], var1[week], xlim = range(xaxis),
             ylim = setRange(var1, mon), panel.first = grid(nx = NA, ny = NULL),
             type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
        abline(v = xaxis, col = "grey", lty = 3)
        axis(2, line = 1.8, padj = 0.9, tcl = -0.3)
        lines(time[week], var1[week], lwd = 2, col = col_comb[1])
        par(new = TRUE)
        plot(time[week], var2[week],
             xlim = range(xaxis), ylim = setRange(var2, mon),
             type = "l", col = col_comb[2], lwd = 2,
             xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        axis(4, padj = -0.9, tcl = -0.3)
        mtext(paste(a, wrap(units[a])), 2, line = 3.6)
        mtext(b, 4, line = 2)
        mtext(wrap(units[b]), 4, line = 3.6, cex = 0.8)
        if (i %% 2 == 1) {
          legend("topleft", legend = c(a, b), lty = 1, lwd = 2,
                 col = col_comb, bty = "n")
        }
      }
    }
    # Graphical display of data in weekly periods===============================
    # Number of intervals with right side closure (+1)
    nInt <- length(seq(from = day1, to = date[nrow(x)], by = "1 week")) + 1L
    # Create weekly intervals for plotting
    int <- seq(from = day1, length.out = nInt, by = "1 week")
    # Compute xlim for barplot (60 * 24 = 1440: minutes in day; 7 days in week)
    tdiff <- as.numeric(time[2] - time[1])
    barxaxis <- 1440 / tdiff * 7
    # Only 6 plots are printed, slots 7 and 8 reserved for margins
    panels <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 7, 7, 4, 4, 4, 5, 5, 5, 6, 6, 6, 8)
    def_par <- par(no.readonly = TRUE)
    par(mar = c(0, 0, 0, 0), oma = c(2.5, 6, 1, 6))
    for (i in 1:(nInt - 1L)) {
      if (i %% 2 == 1) layout(panels)
      week <- date >= int[i] & date < int[i + 1L]
      if (!length(time[week])) next
      center <- int[i] + 3.5
      mon <- date >= (center - 15) & date < (center + 15)
      # xaxis has to be one day longer to simplify plotting
      xaxis <- seq(from = int[i], to = int[i + 1], by = "1 day")
      xaxis <- as.POSIXct(strptime(xaxis, "%Y-%m-%d", tz = "GMT"))
      if (panel_top %in% c("blue_red", "violet_orange")) {
        modules[["two_vars"]](panel_top_vars[1], panel_top_vars[2], panel_top)
      } else modules[[panel_top]]()
      if (document && i %% 2 == 0) {
        mtext(paste("qc_flag = '", qc_flag, "', test = '", test, "'",
                    sep = ""), side = 3, line = 0, adj = 0, cex = 0.6)
      }
      y <- vals[use]
      f <- mon[use]
      if (flux_gf != "none") {
        y <- c(y, vals_gf)
        f <- c(f, mon)
      }
      if (NEE_sep) {
        y <- c(y, Reco, GPP)
        f <- c(f, rep(mon, 2))
      }
      y_range <- setRange(y, f)
      yRange <- c(max(ylim[1], y_range[1], na.rm = TRUE),
                  min(ylim[2], y_range[2], na.rm = TRUE))
      plot(time[week], vals[week], type = "n", xaxt = "n", yaxt = "n",
           ylab = "", xlab = "", panel.first = grid(nx = NA, ny = NULL),
           xlim = range(xaxis), ylim = yRange)
      abline(v = xaxis, col = "grey", lty = 3)
      axis(2, padj = 0.9, tcl = -0.3)
      if (!is.null(P)) {
        par(new = TRUE)
        barplot(P[week], ylim = rev(setRange(P, mon)), xlim = c(0, barxaxis),
                col = "lightskyblue", space = 0, border = NA,
                xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        axis(4, line = 1.8, padj = -0.9, tcl = -0.3)
        mtext(paste("P", wrap(units["P"])), 4, line = 3.6)
      }
      par(new = TRUE)
      plot(time[week], vals[week], xlim = range(xaxis),
           ylim = yRange, type = "o", pch = 19, cex = 0.75,
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "grey")
      # keep the lenght of time[week] but remove excluded data
      # (needed for plotting lines)
      showVals <- vals[week]
      showVals[!use[week]] <- NA
      lines(time[week], showVals, type = "o", col = "black",
            pch = 19, cex = 0.75)
      if (flux_gf != "none") {
        lines(time[week], vals_gf[week],
              type = "l", pch = 19, cex = 0.4, col = "forestgreen")
      }
      if (NEE_sep) {
        lines(time[week], Reco[week],
              type = "l", pch = 19, cex = 0.4, col = "firebrick3")
        lines(time[week], GPP[week],
              type = "l", pch = 19, cex = 0.4, col = "dodgerblue3")
      }
      points(time[week & (exalt == 1)], vals[week & (exalt == 1)],
             col = "green", pch = 19, cex = 0.4)
      points(time[week & (exalt == 2)], vals[week & (exalt == 2)],
             col = "red", pch = 19, cex = 0.4)
      mtext(wrap(units[flux]), 2, line = 2, cex = 0.8)
      mtext(flux, 2, line = 3.6)
      if (i %% 2 == 1) {
        legend("topleft",
               col = c("black", "grey", "green", "red",
                       if (is.null(P)) NULL else "lightskyblue"),
               legend = c("Used data", "Excluded data", "Test flag = 1",
                          "Test flag = 2", if (is.null(P)) NULL else "P"),
               bty = "n", pch = c(19, 19, 19, 19, if (is.null(P)) NULL else 15),
               lty = c(1, 1, NA, NA, if (is.null(P)) NULL else NA))
        if (flux_gf != "none" || NEE_sep) {
          legend("topright",
                 legend = c("Gap-filled data", if (NEE_sep) c("Reco", "GPP")),
                 col = c("forestgreen",
                         if (NEE_sep) c("firebrick3", "dodgerblue3")),
                 lwd = 2, bty = "n")
        }
      }
      if (panel_bottom %in% c("blue_red", "violet_orange")) {
        modules[["two_vars"]](panel_bottom_vars[1], panel_bottom_vars[2],
                              panel_bottom)
      } else modules[[panel_bottom]]()
      # Halfday shift of ticks to mark the middle of day
      axis.POSIXct(1, at = seq(min(xaxis), max(xaxis), by = "days") + 12 * 3600,
                   format = "%Y/%m/%d", padj = -0.5)
      if (i %% 2 == 0) mtext("Day of year", 1, line = 3, cex = 1.2)
    }
    par(def_par)
  }
}

#' Plots for Cursory Time Series Data Overview
#'
#' Plot half-hourly time series data of selected variable as a scatter plot with
#' additional settings. Plots are optimized for quick rendering when saved as
#' PDF files.
#'
#' \code{plot_precheck} allows to glimpse through the preliminary data with
#' outlying data removed by defined \code{qrange}. If you do not want to limit
#' y-axis, set \code{qrange = NULL} or \code{qrange = c(0, 1)} or use
#' \code{plot_hh}.
#'
#' \code{plot_hh} provides a glimpse at all available time series data for given
#' variable.
#'
#' @param x A data frame with column names and \code{"timestamp"} column in
#'   POSIXt format.
#' @param var A character string. An \code{x} column name of the variable to
#'   plot on y-axis.
#' @param qrange A numeric vector of length 2, giving the quantile range of
#'   y-axis.
#' @param pch Either an integer specifying a symbol or a single character to be
#'   used as the default in plotting points. See \code{\link{par}} for details.
#' @param cex A numerical value giving the amount by which plotting text and
#'   symbols should be magnified relative to the default. See \code{\link{par}}
#'   for details.
#' @param alpha.f A numeric value. Factor modifying the color opacity alpha for
#'   plotted points; typically in \code{[0,1]}.
#' @param units A character string. One of the units listed: \code{c("secs",
#'   "mins", "hours", "days", "months", "years")}. Can be abbreviated. Specifies
#'   the rounding applied to first and last record in \code{"timestamp"} column
#'   of \code{x} to produce sensible x-axis ticks and labels.
#' @param interval An interval of the x-axis ticks. See \code{by} argument of
#'   \code{\link{seq.POSIXt}} for details. Intervals are counted from the first
#'   record in \code{"timestamp"} column of \code{x}.
#' @param format A character string defining the date-time information format at
#'   x-axis.see \code{\link{strptime}}.
#'
#' @examples
#' set.seed(123)
#' n <- 17520 # number of half-hourly records in one non-leap year
#' tstamp <- seq(c(ISOdate(2021,3,20)), by = "30 mins", length.out = n)
#' x <- data.frame(timestamp = tstamp, H = rf(n, 1, 2, 1))
#' openeddy::units(x) <- c("", "W m-2")
#' plot(H ~ timestamp, x)
#' plot_hh(x, "H")
#' plot_precheck(x, "H")
#' plot_precheck(x, "H", units = "days", interval = "2 months", format = "%d-%b")
#'
#' @importFrom graphics axis.POSIXct
#' @importFrom grDevices adjustcolor
#' @export
plot_precheck <- function(x,
                          var,
                          qrange = c(0.005, 0.995),
                          pch = ".",
                          cex = 0.5,
                          alpha.f = 0.5,
                          units = "months",
                          interval = "month",
                          format = "%b-%y") {
  if (is.null(x$timestamp)) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  ylim <- x[, var]
  if (!is.null(qrange)) ylim <- quantile(ylim, qrange, na.rm = TRUE)
  ylim <- setRange(ylim)
  plot(x$timestamp, x[, var], ylim = ylim,
       pch = pch, cex = cex, col = adjustcolor('black', alpha.f),
       xaxt = "n", xlab = "timestamp",
       ylab = paste0(var, " [", units(x[var]), "]"),
       main = paste0(var,
                     "; quantile range = ",
                     ifelse(is.null(qrange),
                            "NULL",
                            paste0("c(",
                                   paste(qrange, collapse = ", "),
                                   ")")
                     )
       )
  )
  r <- as.POSIXct(round(range(x$timestamp), units))
  axis.POSIXct(1, at = seq(r[1], r[2], by = interval), format = format)
}

#' @rdname plot_precheck
#'
#' @importFrom graphics axis.POSIXct
#' @importFrom grDevices adjustcolor
#' @export
plot_hh <- function(x, var, pch = ".", cex = 1, alpha.f = 1, units = "months",
                    interval = "month", format = "%b-%y") {
  if (is.null(x$timestamp)) stop("missing 'x$timestamp'")
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  # assure plotting will be successful even without any value in var
  ylim <- setRange(x[, var])
  plot(x$timestamp, x[, var], pch = pch, cex = cex, xaxt = "n",
       ylim = ylim,
       col = adjustcolor('black', alpha.f),
       xlab = "timestamp",
       ylab = paste0(var, " [", units(x[var]), "]"),
       main = var)
  r <- as.POSIXct(round(range(x$timestamp), units))
  axis.POSIXct(1, at = seq(r[1], r[2], by = interval), format = format)
}

#' Bar Plots of Aggregated Variables
#'
#' Creates a bar plot with aggregated variable on y-axis and labels of
#' aggregation periods on x-axis.
#'
#' @param x A data frame with column names.
#' @param var A character string. An \code{x} column name of the variable to
#'   plot on y-axis.
#' @param interval A character string. Specifies \code{xlab} timescale and does
#'   not affect computations (e.g. "daily", "3-daily", "weekly", "monthly",
#'   etc.).
#' @param nTicks An integer. Number of x-axis ticks to plot. If \code{NULL},
#'   maximum 20 ticks with corresponding \code{names.arg} will be plotted,
#'   otherwise \code{nTicks} defaults to \code{8}. Specifying \code{nTicks}
#'   allows greater control over the x-axis ticks density.
#' @param days A numeric vector. Number of days (or their fractions) aggregated
#'   within each time interval described by \code{names.arg}. Used to specify
#'   bar widths.
#' @param names.arg A character vector. Names of each aggregation period
#'   corresponding to \code{x$var} used as x-axis labels.
#'
#' @seealso \code{\link{barplot}}
#'
#' @examples
#' set.seed(123)
#' n <- 17520 # number of half-hourly records in one non-leap year
#' tstamp <- seq(c(ISOdate(2021,3,20)), by = "30 mins", length.out = n)
#' x <- data.frame(timestamp = tstamp, H = rf(n, 1, 2, 1))
#' aggm <- agg_sum(x, "%b-%Y")
#' barplot_agg(aggm, var = "H_sum", "monthly")
#' aggw <- agg_sum(x, "%W_%Y")
#' barplot_agg(aggw, var = "H_sum", "weekly")
#' aggd <- agg_sum(x, "%Y-%m-%d")
#' barplot_agg(aggd, var = "H_sum", "daily")
#'
#' @importFrom graphics barplot axis box
#' @export
barplot_agg <- function(x, var, interval = NULL, nTicks = NULL, days = x$days,
                        names.arg = x$Intervals) {
  # assure plotting will be successful even without any value in var
  ylim <- setRange(x[, var])
  p <- barplot(as.numeric(x[, var]),
               space = 0,
               width = days,
               col = "grey",
               border = "grey35",
               ylim = if (var %in% c("evaporative_fraction",
                                     "closure_fraction"))
                 c(0, 1) else ylim,
               xlab = if (!is.null(interval)) paste(interval, "timescale"),
               ylab = paste0(var, " [", units(x[var]), "]"),
               main = var,
               xpd = FALSE)
  if (is.null(nTicks)) {
    if (length(p) <= 20) {
      axis(1, at = p, labels = names.arg)
    } else {
      index <- as.integer(seq(1, length(p), length.out = 8))
      axis(1, at = p[index], labels = names.arg[index])
    }
  } else {
    if (nTicks > length(p)) stop("nTicks larger than length(names.arg)")
    index <- as.integer(seq(1, length(p), length.out = nTicks))
    axis(1, at = p[index], labels = names.arg[index])
  }
  box()
}

#' Create a New ggplot with Statistics
#'
#' Create a ggplot object representing a scatter plot with statistics for given
#' amount of intervals along x-axis. Each interval contains comparable amount of
#' data points, thus can have unequal width.
#'
#' \code{circular = TRUE} effectively sets the last interval as neighboring to
#' the first interval. This allows to interpolate the statistics also for the
#' edge cases.
#'
#' \code{qrange} reduces y-axis limits to reduce the impact of outliers on plot
#' readability. It does not affect computed statistics. If you do not want to
#' limit y-axis, set \code{qrange = NULL} or \code{qrange = c(0, 1)}.
#'
#' @param data A data frame with required timestamp column
#'   (\code{data$timestamp}) of class \code{"POSIXt"}.
#' @param x,y A character string. A \code{data} column name of the variable to
#'   plot on x-axis (y-axis).
#' @param breaks An integer. Number of breakpoints separating variable \code{x}
#'   to \code{breaks - 1} intervals.
#' @param circular A logical value. Is \code{x} a circular variable?
#' @param ylim Either \code{NULL}, \code{"band"} or a numeric vector of length
#'   2. If \code{NULL} (default), y-axis limits are taken from the original
#'   data, potentially modified by \code{qrange}. If \code{"band"}, y-axis
#'   limits are chosen to fit only the computed uncertainty band. If numeric
#'   vector, manually specified limits are applied. In latter two cases
#'   \code{qrange} is ignored.
#' @param qrange A numeric vector of length 2, giving the quantile range of
#'   y-axis.
#' @param center,deviation A character string. Statistics applied to each x-axis
#'   interval for computation of its center (deviation) along y-axis.
#' @param header A logical value. Should automated plot title and subtitle be
#'   included?
#'
#' @seealso \code{\link{aggregate}}, \code{\link{as.POSIXlt}},
#'   \code{\link{cut.POSIXt}}, \code{\link{mean}}, \code{\link{regexp}},
#'   \code{\link{strftime}}, \code{\link{sum}}, \code{\link{timezones}},
#'   \code{\link{varnames}}
#'
#' @examples
#' set.seed(123)
#' n <- 17520 # number of half-hourly records in one non-leap year
#' tstamp <- seq(c(ISOdate(2021,3,20)), by = "30 mins", length.out = n)
#' x <- data.frame(timestamp = tstamp,
#'                 wd = seq(0,360, length.out = n),
#'                 H = rf(n, 1, 2, 1))
#' openeddy::units(x) <- c("", "deg", "W m-2")
#' ggplot_stats(x, x = "wd", y = "H", qrange = c(0.005, 0.9))
#' ggplot_stats(x, x = "wd", y = "H", circular = TRUE, qrange = c(0.005, 0.9))
#' ggplot_stats(x, x = "wd", y = "H", ylim = "band")
#'
#' @importFrom stats mad sd approx
#' @importFrom grDevices hcl.colors
#' @export
ggplot_stats <- function(data, x, y, breaks = 20, circular = FALSE, ylim = NULL,
                         qrange = c(0.005, 0.995), center = c("median", "mean"),
                         deviation = c("mad", "sd"), header = TRUE) {
  if (is.null(data$timestamp)) stop("missing 'data$timestamp'")
  if (!inherits(data$timestamp, "POSIXt")) {
    stop("'data$timestamp' must be of class 'POSIXt'")
  }
  center_name <- match.arg(center)
  center <- match.fun(center_name)
  deviation_name <- match.arg(deviation)
  deviation <- match.fun(deviation_name)
  units_x <- units(data[x])
  units_y <- units(data[y])
  data <- na.omit(data[c("timestamp", x, y)])
  # including also mid points in sequence
  sq <- quantile(data[, x], seq(0, 1, len = breaks*2-1), na.rm = TRUE)
  l <- split(data[, y], cut(data[, x], sq[c(TRUE, FALSE)]))
  df <- data.frame(wind_dir = c(sq[1], sq[c(FALSE, TRUE)], sq[length(sq)]),
                   cen = c(center(l[[1]], na.rm = TRUE),
                           sapply(l, center, na.rm = TRUE),
                           center(l[[length(l)]], na.rm = TRUE)),
                   dev = c(deviation(l[[1]], na.rm = TRUE),
                           sapply(l, deviation, na.rm = TRUE),
                           deviation(l[[length(l)]], na.rm = TRUE)))
  if (circular) {
    df$cen[c(1, nrow(df))] <- mean(df$cen[c(1, nrow(df))], na.rm = TRUE)
    df$dev[c(1, nrow(df))] <- mean(df$dev[c(1, nrow(df))], na.rm = TRUE)
  }
  edge <- data.frame(wind_dir = sq[c(TRUE, FALSE)],
                     edg = approx(df$wind_dir, df$cen, sq[c(TRUE, FALSE)])$y,
                     dev = approx(df$wind_dir, df$dev, sq[c(TRUE, FALSE)])$y)
  edge <- na.omit(edge)
  # case when ylim = NULL (original data range or qrange)
  if (is.null(ylim)) {
    ylim <- data[, y]
    if (!is.null(qrange)) ylim <- quantile(ylim, qrange, na.rm = TRUE)
    ylim <- setRange(ylim)
  } else if ("band" %in% ylim) {
    # case when ylim should reflect only the uncertainty band, not all data
    ylim <- range(edge$edg + edge$dev, edge$edg - edge$dev, na.rm = TRUE)
  } # in other cases use ylim as is (implicit)
  val <- center(data[, y], na.rm = TRUE)
  cline <- data.frame(x_lim = df$wind_dir[c(1, nrow(df))], y = val)
  data$DOY <- as.POSIXlt(data$timestamp)$yday + 1
  ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$DOY),
      size = 1,
      na.rm = TRUE,
      alpha = 0.5
    ) +
    ggplot2::scale_colour_gradientn(
      colours = c(hcl.colors(10), rev(hcl.colors(10)))
    ) +
    ggplot2::geom_ribbon(
      data = df, inherit.aes = FALSE,
      ggplot2::aes(x = .data$wind_dir,
                   ymin = .data$cen - .data$dev,
                   ymax = .data$cen + .data$dev,
                   fill = !!deviation_name),
      color = "grey30", alpha = 0.5, size = 0.8
    ) +
    ggplot2::geom_linerange(
      data = edge, inherit.aes = FALSE,
      ggplot2::aes(x = .data$wind_dir,
                   ymin = .data$edg - .data$dev,
                   ymax = .data$edg + .data$dev),
      color = "grey30", size = 0.6
    ) +
    ggplot2::geom_line(data = cline,
                       ggplot2::aes(x = .data$x_lim, y = .data$y),
                       color = "grey30", size = 0.8) +
    ggplot2::geom_point(data = df[-c(1, nrow(df)),],
                        ggplot2::aes(.data$wind_dir, .data$cen,
                                     alpha = !!center_name),
                        color = "black", size = 2) +
    ggplot2::geom_line(data = df,
                       ggplot2::aes(.data$wind_dir, .data$cen),
                       color = "black", size = 0.8) +
    ggplot2::scale_fill_manual(name = NULL, values = "white") +
    ggplot2::scale_alpha_manual(name = NULL, values = 1) +
    ggplot2::guides(alpha = ggplot2::guide_legend(order = 1),
                    fill = ggplot2::guide_legend(order = 2),
                    colorbar = ggplot2::guide_legend(order = 3)) +
    ggplot2::annotate("label", size = 6, x = Inf, y = Inf, hjust = 1, vjust = 1,
                      label = paste(center_name, y, "=", round(val, 3)),
                      fill = "white", alpha = 0.8, label.size = NA) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = paste0(x, " [", units_x, "]"),
                  y = paste0(y, " [", units_y, "]")) +
    if (header == TRUE) {
      ggplot2::labs(
        title = paste0("Dependence of ", y, " on ", x),
        subtitle = paste0("quantile range = ",
                          ifelse(is.null(qrange),
                                 "NULL",
                                 paste0("c(",
                                        paste(qrange, collapse = ", "),
                                        ")")
                          )
        )
      )
    } else NULL
}
