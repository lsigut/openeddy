#' Apply Thresholds
#'
#' Values of \code{x} are checked against two specified thresholds to obtain
#' their quality control (QC) flags.
#'
#' This function is called by \code{\link{extract_QC}} but can be useful on its
#' own when filtering values of variable according to the 0 - 2 QC flag scheme.
#'
#' Obtained QC flags are assigned in the range 0 - 2 according to these rules:
#'
#' For \code{flag = "higher"} \itemize{ \item If \code{x <= thr[1]}, QC flag =
#' 0. \item If \code{x > thr[1] & x <= thr[2]}, QC flag = 1. \item If \code{x >
#' thr[2]}, QC flag = 2.}
#'
#' For \code{flag = "outside"} \itemize{ \item If \code{x >= thr[1] & x <=
#' thr[2]}, QC flag = 0. \item If \code{x < thr[1] | x > thr[2]}, QC flag = 2.}
#'
#' For \code{flag = "between"} \itemize{ \item If \code{x <= thr[1] | x >=
#' thr[2]}, QC flag = 0. \item If \code{x > thr[1] & x < thr[2]}, QC flag = 2.
#' }
#'
#' For \code{flag = "lower"} \itemize{ \item If \code{x >= thr[1]}, QC flag = 0.
#' \item If \code{x < thr[1] & x >= thr[2]}, QC flag = 1. \item If \code{x <
#' thr[2]}, QC flag = 2.}
#'
#' @return An integer vector with the same length as \code{x}. Its
#'   \code{varnames} and \code{units} attributes are set to  \code{name_out} and
#'   \code{"-"} values, respectively.
#'
#' @param x A numeric atomic type with \code{NULL} \code{\link{dim}}ensions.
#' @param thr A numeric vector with 2 non-missing values.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#' @param flag A character string. Selects one of the available flagging
#'   approaches. Can be abbreviated. See 'Details'.
#'
#' @seealso \code{\link{extract_QC}}.
#'
#' @examples
#' apply_thr(1:10, c(3, 6), "example")
#' set.seed(1)
#' xx <- data.frame(var = rnorm(20, mean = 1, sd = 2))
#' xx$higher <- apply_thr(xx$var, c(0, 1), "higher", flag = "higher")
#' xx$outside <- apply_thr(xx$var, c(-1, 1), "outside", flag = "outside")
#' xx$between <- apply_thr(xx$var, c(-1, 1), "between", flag = "between")
#' xx$lower <- apply_thr(xx$var, c(0, -1), "lower", flag = "lower")
#' xx
#' str(xx)
apply_thr <- function(x, thr, name_out = "-",
                      flag = c("higher", "outside", "between", "lower")) {
  if (!is.numeric(x)) stop("'x' must be numeric")
  # matrix and array is numeric - we do not want them:
  if (!is.null(dim(x))) stop("'dim(x)' must be NULL")
  if (!is.numeric(thr) || length(thr) != 2 || anyNA(thr)) {
    stop("'thr' must be numeric vector with 2 non-missing values")
  }
  if (!is.atomic(name_out) || length(name_out) != 1) {
    stop("atomic type 'name_out' must have length 1")
  }
  name_out <- if (name_out %in% c("", NA)) "-" else as.character(name_out)
  flag <- match.arg(flag)
  out <- rep(NA, length(x))
  if (flag == "higher") {
    if (thr[1] > thr[2]) stop("'thr[1]' cannot be higher than 'thr[2]'")
    out[x <= thr[1]] <- 0L
    out[x >  thr[1]] <- 1L
    out[x >  thr[2]] <- 2L
  }
  if (flag == "outside") {
    if (thr[1] > thr[2]) stop("'thr[1]' cannot be higher than 'thr[2]'")
    out[x >= thr[1] & x <= thr[2]] <- 0L
    out[x < thr[1] | x > thr[2]] <- 2L
  }
  if (flag == "between") {
    if (thr[1] > thr[2]) stop("'thr[1]' cannot be higher than 'thr[2]'")
    out[x <= thr[1] | x >= thr[2]] <- 0L
    out[x > thr[1] & x < thr[2]] <- 2L
  }
  if (flag == "lower") {
    if (thr[1] < thr[2]) stop("'thr[1]' cannot be lower than 'thr[2]'")
    out[x >= thr[1]] <- 0L
    out[x <  thr[1]] <- 1L
    out[x <  thr[2]] <- 2L
  }
  attributes(out) <- list(varnames = name_out, units = "-")
  return(out)
}

#' Flag Runs of Equal Values
#'
#' Identify and flag values of runs with repeating values in a vector.
#'
#' \code{NA} values are omitted before evaluation of runs. Thus \code{NA}s do
#' not interrupt runs. Flagging is done according to the 0 - 2 quality control
#' flag scheme.
#'
#' @param x A numeric atomic type with NULL dimensions.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#' @param length A numeric value.
#'
#' @return An integer vector with the same length as \code{x}. Its
#'   \code{varnames} and \code{units} attributes are set to  \code{name_out} and
#'   \code{"-"} values, respectively.
#'
#' @examples
#' (xx <- c(rep(c(0, NA), 5)))
#' flag_runs(xx, "qc_xx_runs")
#' (yy <- rep(1:6, rep(c(2, 1), 3)))
#' flag_runs(yy, "qc_yy_runs")
flag_runs <- function(x, name_out = "-", length = 2) {
  if (!is.numeric(x)) stop("'x' must be numeric")
  # matrix and array is numeric - we do not want them:
  if (!is.null(dim(x))) stop("'dim(x)' must be NULL")
  if (!is.atomic(name_out) || length(name_out) != 1) {
    stop("atomic type 'name_out' must have length 1")
  }
  name_out <- if (name_out %in% c("", NA)) "-" else as.character(name_out)
  if (!is.numeric(length) || length(length) != 1 || is.na(length)) {
    stop("'length' must be non-missing numeric value")
  }
  out <- rep(NA, length(x))
  not_NA <- !is.na(x)
  x <- x[not_NA]
  rl <- rle(x)$lengths
  keep <- rl >= length
  run_start <- (cumsum(rl) + 1 - rl)[keep]
  run_end <- cumsum(rl)[keep]
  df <- data.frame(run_start, run_end)
  runs <- unlist(apply(df, 1, function(x) x[1]:x[2]))
  out[not_NA] <- 0L
  out[not_NA][runs] <- 2L
  attributes(out) <- list(varnames = name_out, units = "-")
  return(out)
}

#' Extract Quality Control Information from Coded Values
#'
#' This function is called by \code{\link{extract_QC}} and is not inteded to be
#' used directly. QC information for multiple variables stored in a character
#' vector of certain properties is extracted and interpreted.
#'
#' Each element of \code{x} is expected to have the same structure according to
#' the format specified by \code{units} attribute of \code{x}. \code{units}
#' format string (e.g. \code{"8u/v/w/ts/h2o/co2"}) starts with a prefix
#' (\code{"8"}) that is followed by variable names (\code{"u", "v", "w", "ts",
#' "h2o", "co2"}) that are distinguished by a separator (\code{"/"}). Elements
#' of \code{x} thus consist of either seven components (prefix and six flags for
#' each variable) or one \code{NA} value. Flags can have three values: \itemize{
#' \item Flag 0: measured variable passed the test. \item Flag 1: measured
#' variable failed the test. \item Flag 9: flag could not be obtained for the
#' given variable (\code{NA}).}
#'
#' \code{x} is interpreted in respect to instruments required to measure given
#' fluxes. SA is required for measurements of all fluxes and solely provides
#' data for computation of Tau and H fluxes. A combination of SA and IRGA
#' (SA_IRGA) is needed for measurements of LE and NEE. To confirm the correct
#' performance of SA, all variables \code{"u", "v", "w", "ts"} must have flag 0.
#' In case of SA_IRGA, all variables \code{"u", "v", "w", "ts", "h2o", "co2"}
#' must have flag 0. Results are reported according to the QC scheme using QC
#' flag range 0 - 2.
#'
#' @section Abbreviations: \itemize{ \item QC: Quality Control \item SA: Sonic
#'   Anemometer \item IRGA: InfraRed Gas Analyzer \item Tau: Momentum flux [kg
#'   m-1 s-2] \item H: Sensible heat flux [W m-2] \item LE: Latent heat flux [W
#'   m-2] \item NEE: Net ecosystem exchange [umol m-2 s-1] \item u: Longitudinal
#'   wind speed component [m s-1] \item v: Cross-wind wind speed component [m
#'   s-1] \item w: Vertical wind speed component [m s-1] \item ts: Sonic
#'   temperature [degC] \item h2o: H2O concentration [mmol mol-1] \item co2: CO2
#'   concentration [umol mol-1]}.
#'
#' @return A data frame with columns \code{"SA"} and \code{"SA_IRGA"}. Each
#'   column has attributes \code{"varnames"} and \code{"units"} and length equal
#'   to that of \code{x}.
#'
#' @param x An atomic type.
#' @param prefix Character string containing a \code{\link{regular expression}}
#'   identifying the prefix of coded values.
#' @param split Character string containing a \code{\link{regular expression}}
#'   identifying the separator of variable names in \code{units} attribute.
#'
#' @seealso \code{\link{extract_QC}}.
#'
#' @examples
#' set.seed(5)
#' xx <- replicate(10,
#'                 paste0(c("8", sample(c(0:1, 9), 6, c(0.7, 0.2, 0.1),
#'                                     replace = TRUE)),
#'                       collapse = ""))
#' is.na(xx) <- c(2, 5)
#' units(xx) <- "8u/v/w/ts/h2o/co2"
#' cbind(xx, extract_coded(xx))
extract_coded <- function(x, prefix = "[8]", split = "[/]") {
  if (!is.atomic(x)) stop("'x' must be an atomic type")
  units <- units(x)
  vars <- unlist(strsplit(gsub(prefix, "", units),
                          split = split))
  req_vars <- c("u", "v", "w", "ts", "h2o", "co2")
  if (!all(req_vars %in% vars)) {
    stop(paste("coded variables in units of coded vector are missing:",
               paste0(req_vars[!(req_vars %in% vars)], collapse = ", ")))
  }
  # Helper function for abslim and spikesHF (controls column splitting)
  separate <- function(x, ...) {
    strsplit(as.character(gsub(...,"", x)), NULL)
  }
  l <- lapply(x, separate, prefix)
  # Helper function for abslim and spikesHF (controls NAs in splitted columns)
  # 9 is internal code for NA. In case of missing halfhour inserts NAs
  # according to number of variables in 'units'.
  handleNA <- function(x, variables) {
    out <- as.numeric(unlist(x))
    out[out == 9]  <- NA
    length(out) <- length(variables)
    return(out)
  }
  df <- as.data.frame(t(sapply(l, handleNA, vars)))
  names(df) <- vars
  out <- data.frame(SA = apply(df[c("u", "v", "w", "ts")], 1, max))
  out$SA_IRGA <- apply(df[c("u", "v", "w", "ts", "h2o", "co2")], 1, max)
  # values above 0 are interpreted as flag 2 for given variable
  out$SA[out$SA == 1]           <- 2L
  out$SA_IRGA[out$SA_IRGA == 1] <- 2L
  for (i in seq_len(ncol(out))) {
    varnames(out[, i]) <- c("SA", "SA_IRGA")[i]
    units(out[, i]) <- "-"
  }
  return(out)
}

#' Extract Quality Control Information
#'
#' QC information stored in columns of data frame \code{x} is extracted and
#' interpreted in respect to instruments or individual fluxes.
#'
#' The data frame \code{x} is expected to have certain properties. It is
#' required that it contains column names according to considered QC checks. See
#' 'Arguments' above. Attribute \code{units} is required for columns
#' \code{"absolute_limits_hf" and "spikes_hf"} to extract the coded QC
#' information. See '\code{\link{extract_coded}}'.
#'
#' Extracted QC information can be relevant to fluxes measured by given
#' instrument(s), specific flux or is applicable to all fluxes. See 'Naming
#' Strategy' below. Results are reported according to the QC scheme using QC
#' flag range 0 - 2. In cases when extracted variable is checked against
#' thresholds (missfrac, scf, wresid), \code{\link{apply_thr}} is used to assign
#' flag values. First value of \code{missfrac_thr}, \code{scf_thr},
#' \code{w_unrot_thr} or \code{w_rot_thr} sets threshold for flag 1, second
#' value sets threshold for flag 2. If both threshold values are the same, only
#' flag 0 and 2 will be resolved (hard flag).
#'
#' Check of missing data in averaging period (missfrac) takes into account
#' number of valid records used for given averaging period. This number is
#' further reduced by the sum of count of high frequency data spikes out of
#' variables needed to compute covariance. Covariance pairs are w, u (Tau); w,
#' ts (H); w, h2o (LE) and w, co2 (NEE).
#'
#' @section Extracted QC Checks: \itemize{ \item Check of plausibility limits
#'   (abslim). Test is not additive. \item Check of high frequency data spike
#'   percentage in averaging period against thresholds (spikesHF). Test is not
#'   additive. \item Check of missing data in averaging period against
#'   thresholds (missfrac). Test is not additive. \item Check of spectral
#'   correction factor against thresholds (scf). Test is not additive. \item
#'   Check of mean unrotated w (double rotation) or w residual (planar fit)
#'   against thresholds (wresid). Additive test.}
#'
#' @section Content and Format of Columns: \itemize{ \item
#'   \code{"absolute_limits_hf"}: hard flags (passed or failed the test) for
#'   individual variables for absolute limits. Limits for each variable are set
#'   in the post-processing software which also reports the resulting flags in a
#'   coded format. See '\code{\link{extract_coded}}'. \item \code{"spikes_hf"}:
#'   hard flags (passed or failed the test) for individual variables for spike
#'   test. Threshold for maximum allowed percentage of spikes within averaging
#'   period for all variables is set in the post-processing software which also
#'   reports the resulting flags in a coded format. See
#'   '\code{\link{extract_coded}}'. \item \code{"file_records"}: number of valid
#'   records found in the raw file \item \code{"used_records"}: number of valid
#'   records used at given averaging period. This number can also contain high
#'   frequency spikes that should be excluded. \item \code{"u_spikes",
#'   "ts_spikes", "h2o_spikes" and "co2_spikes"}: number of high frequency
#'   spikes detected at given averaging period in respective variable. Values
#'   can be set to 0 if \code{"used_records"} already accounts for spikes. \item
#'   \code{"Tau_scf", "H_scf", "LE_scf" and "co2_scf"}: spectral correction
#'   factor for given flux. Values are above 1. \item \code{"w_unrot", "w_rot"}:
#'   unrotated and rotated w wind component, respectively (should be close to
#'   0).}
#'
#' @section Naming Strategy: \strong{QC prefixes} (specifies which flux is
#'   affected by that QC output): \itemize{ \item qc_SA: applicable to fluxes
#'   relying only on SA (Tau, H) \item qc_SA_IRGA: applicable to fluxes relying
#'   both on SA and IRGA (LE, NEE) \item qc_Tau, qc_H, qc_LE, qc_NEE: only
#'   applicable for the respective flux \item qc_ALL: applicable to all fluxes}
#'
#'   \strong{QC suffixes} (specifies which QC check was applied to get this QC
#'   output): \itemize{ \item abslim, spikesHF, missfrac, wresid. See 'Included
#'   QC Checks' above.}
#'
#' @section Abbreviations: \itemize{ \item QC: Quality Control \item SA: Sonic
#'   Anemometer \item IRGA: InfraRed Gas Analyzer \item Tau: Momentum flux [kg
#'   m-1 s-2] \item H: Sensible heat flux [W m-2] \item LE: Latent heat flux [W
#'   m-2] \item NEE: Net ecosystem exchange [umol m-2 s-1] \item u: Longitudinal
#'   wind speed component [m s-1] \item w: Vertical wind speed component [m s-1]
#'   \item ts: Sonic temperature [degC] \item h2o: H2O concentration [mmol
#'   mol-1] \item co2: CO2 concentration [umol mol-1]}.
#'
#' @section References: Foken, T., Wichura, B., 1996. Tools for quality
#'   assessment of surface-based flux measurements. Agric. For. Meteorol. 78,
#'   83–105. doi:10.1016/0168-1923(95)02248-1
#'
#'   Mauder, M., Cuntz, M., Drue, C., Graf, A., Rebmann, C., Schmid, H.P.,
#'   Schmidt, M., Steinbrecher, R., 2013. A strategy for quality and uncertainty
#'   assessment of long-term eddy-covariance measurements. Agric. For. Meteorol.
#'   169, 122-135. doi:10.1016/j.agrformet.2012.09.006
#'
#' @return A data frame. Each column has attributes \code{"varnames"} and
#'   \code{"units"} .
#'
#' @param x A data frame with column names representing required variables.
#' @param abslim A logical value. Determines whether plausibility limits check
#'   should be considered. If \code{abslim = TRUE}, column
#'   \code{"absolute_limits_hf"} is required in \code{x}.
#' @param spikesHF A logical value. Determines whether check of spike percentage
#'   in averaging period should be considered. If \code{spikesHF = TRUE}, column
#'   \code{"spikes_hf"} is required in \code{x}.
#' @param missfrac A logical value. Determines whether check of missing data in
#'   averaging period against thresholds should be done. If \code{missfrac =
#'   TRUE}, columns \code{"file_records", "used_records", "w_spikes",
#'   "u_spikes", "ts_spikes", "h2o_spikes" and "co2_spikes"} are required in
#'   \code{x}.
#' @param scf A logical value. Determines whether check of spectral correction
#'   factor against thresholds should be done. If \code{scf = TRUE}, columns
#'   \code{"Tau_scf", "H_scf", "LE_scf" and "co2_scf"} are required in \code{x}.
#' @param wresid A logical value. Determines whether check of mean unrotated w
#'   and w residual after planar fit against thresholds should be done. If
#'   \code{wresid = TRUE}, columns \code{"w_unrot"} and \code{"w_rot"} are
#'   required in \code{x}.
#' @param rotation A character string. Specifies the type of coordinate rotation
#'   applied. Allowed values are "double" and "planar fit". Can be abbreviated.
#' @param prefix Character string containing a \code{\link{regular expression}}
#'   identifying the prefix of coded values. See '\code{\link{extract_coded}}'.
#' @param split Character string containing a \code{\link{regular expression}}
#'   identifying the separator of variable names in \code{units} attribute. See
#'   '\code{\link{extract_coded}}'.
#' @param missfrac_thr A numeric vector with 2 non-missing values. Represents
#'   thresholds (allowed fraction of missing high frequency data within
#'   averaging period) used if \code{missfrac = TRUE}.
#' @param scf_thr A numeric vector with 2 non-missing values. Represents
#'   thresholds used if \code{scf = TRUE}.
#' @param w_unrot_thr A numeric vector with 2 non-missing values. Represents
#'   thresholds for unrotated w used if \code{wresid = TRUE}.
#' @param w_rot_thr A numeric vector with 2 non-missing values. Represents
#'   thresholds for rotated w used if \code{wresid = TRUE}.
#'
#' @seealso \code{\link{extract_coded}} and \code{\link{apply_thr}}.
extract_QC <- function(x, abslim = TRUE, spikesHF = TRUE, missfrac = TRUE,
                       scf = TRUE, wresid = TRUE, rotation = c("double",
                       "planar fit"), prefix = "[8]", split = "[/]",
                       missfrac_thr = c(0.1, 0.1), scf_thr = c(2, 3),
                       w_unrot_thr = c(0.35, 0.35), w_rot_thr = c(0.1, 0.15)) {
  # Basic check of input =======================================================
  x_names <- colnames(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  req_vars <- character()
  if (abslim) req_vars <- c(req_vars, "absolute_limits_hf")
  if (spikesHF) req_vars <- c(req_vars, "spikes_hf")
  if (missfrac) {
    req_vars <- c(req_vars, "file_records", "used_records", "u_spikes",
                  "w_spikes", "ts_spikes", "co2_spikes", "h2o_spikes")
  }
  if (scf) {
    req_vars <- c(req_vars, "Tau_scf", "H_scf", "LE_scf", "co2_scf")
  }
  if (wresid) {
    req_vars <- c(req_vars, "w_unrot", "w_rot")
  }
  if (length(req_vars) > 0 && !all(req_vars %in% x_names)) {
    stop(paste("missing", paste0(req_vars[!(req_vars %in% x_names)],
                                 collapse = ", ")))
  }
  units <- units(x, names = TRUE)
  if (abslim && units["absolute_limits_hf"] == "-") {
    stop("missing absolute_limits_hf format in its units attribute")
  }
  if (spikesHF && units["spikes_hf"] == "-") {
    stop("missing spikes_hf format in its units attribute")
  }
  # Create output dataframe for saving qc flags
  out <- x[, 0]

  # abslim creates composite flags for H, LE and CO2 based on EddyPro
  # absolute_limits_hf column ==================================================
  if (abslim) {
    abslim_df <- extract_coded(x$absolute_limits_hf, prefix, split)
    for (i in 1:2) {
      varnames(abslim_df[, i]) <- c("qc_SA_abslim", "qc_SA_IRGA_abslim")[i]
    }
    out$qc_SA_abslim <- abslim_df$SA
    out$qc_SA_IRGA_abslim <- abslim_df$SA_IRGA
  }

  # spikesHF creates composite flags for H, LE and CO2 based on EddyPro
  # spikes_hf column ===========================================================
  if (spikesHF) {
    spike_df <- extract_coded(x$spikes_hf, prefix, split)
    for (i in 1:2) {
      varnames(spike_df[, i]) <- c("qc_SA_spikesHF", "qc_SA_IRGA_spikesHF")[i]
    }
    out$qc_SA_spikesHF <- spike_df$SA
    out$qc_SA_IRGA_spikesHF <- spike_df$SA_IRGA
  }

  # missfrac creates overall flag for halfhours with insufficient
  # high frequency readings ====================================================
  if (missfrac) {
    # Flag according to argument 'missfrac_thr' (missing fraction thresholds):
    # missfrac > missfrac_thr[1]: flag = 1, missfrac > missfrac_thr[2]: flag = 2
    if (!all(is.na(x$file_records))) {
      ur <- x$used_records
      mfr <- max(x$file_records, na.rm = TRUE) # maximum file records (mfr)
      # mf computes missing fraction for particular flux
      mf <- function(x, ur, mfr) {
        1 - (ur - apply(x, 1, sum, na.rm = TRUE)) / mfr
      }
      mf_Tau   <- mf(x[c("w_spikes", "u_spikes")], ur, mfr)
      mf_H     <- mf(x[c("w_spikes", "ts_spikes")], ur, mfr)
      mf_LE    <- mf(x[c("w_spikes", "h2o_spikes")], ur, mfr)
      mf_NEE  <- mf(x[c("w_spikes", "co2_spikes")], ur, mfr)
      out$qc_Tau_missfrac <- apply_thr(mf_Tau, missfrac_thr, "qc_Tau_missfrac")
      out$qc_H_missfrac   <- apply_thr(mf_H, missfrac_thr, "qc_H_missfrac")
      out$qc_LE_missfrac  <- apply_thr(mf_LE, missfrac_thr, "qc_LE_missfrac")
      out$qc_NEE_missfrac <- apply_thr(mf_NEE, missfrac_thr, "qc_NEE_missfrac")
    } else {
      warning("'x$file_records' has no non-missing values, skipped missfrac",
              call. = FALSE)
    }
  }

  # scf creates flag for halfhours with excessive spectral correction ==========
  if (scf) {
    # Flag according to argument 'scf_thr' (spectral correction factor
    # thresholds): scf > scf_thr[1]: flag = 1, scf > scf_thr[2]: flag = 2
    nin <- c("Tau_scf", "H_scf", "LE_scf", "co2_scf")
    nout <- c("qc_Tau_scf", "qc_H_scf", "qc_LE_scf", "qc_NEE_scf")
    for (i in seq_along(nout)) {
      out[, nout[i]] <- apply_thr(x[, nin[i]], scf_thr, nout[i])
    }
  }

  # wresid creates overall flag for halfhours with probable advection ==========
  if (wresid) {
    rotation <- match.arg(rotation)
    message(paste("wresid:", rotation, "rotation - using",
                  ifelse(rotation == "double", "w_unrot_thr", "w_rot_thr")))
    out$qc_ALL_wresid <- if (rotation == "double") {
      # In case of double rotation abs(w_unrot) should be < 0.35 m/s
      apply_thr(abs(x$w_unrot), w_unrot_thr, "qc_ALL_wresid")
    } else {
      # Flag correction according to residual absolute w after planar fit
      # abs(w) > 0.10 m s-1: flag incresead by +1, abs(w) > 0.15 m s-1: flag = 2
      apply_thr(abs(x$w_rot), w_rot_thr, "qc_ALL_wresid")
    }
  }
  return(out)
}

#' Flux Interdependency
#'
#' Interdependency of H, LE and NEE QC flags due to corrections/conversions.
#'
#' Flux interdependency is an additive QC flag correction. Results follow the QC
#' scheme using QC flag range 0 - 2. Returned data frame follows the 'Naming
#' Strategy' described in \code{\link{extract_QC}}. Returned QC flags have QC
#' suffix interdep.
#'
#' To convert buoyancy flux to sensible heat flux (SND or Schotanus correction),
#' reliable measurements of LE must be available. To correct LE and NEE
#' estimated by open-path IRGA (\code{IRGA = "open"}) for the effects of density
#' fluctuations due to temperature and humidity fluctuations (WPL or Webb
#' correction), reliable measurements of H must be available. To perform WPL
#' correction of NEE estimated with any kind of IRGA, reliable measurements of
#' LE must be available. Thus following set of rules apply:
#'
#' If \code{IRGA = "en_closed"} \itemize{ \item If \code{qc_LE == 2 |
#' is.na(qc_LE) == TRUE}: qc_H and qc_NEE flags are increased by 1.} If
#' \code{IRGA = "open"} \itemize{ \item if \code{qc_LE == 2 | is.na(qc_LE) ==
#' TRUE}: qc_H flags are increased by 1. \item If \code{qc_H == 2 | is.na(qc_H)
#' == TRUE}: qc_LE flags are increased by 1. \item If \code{qc_H == 2 |
#' is.na(qc_H) == TRUE | qc_LE == 2 | is.na(qc_LE) == TRUE}: qc_NEE flags are
#' increased by 1.}
#'
#' @section Abbreviations: \itemize{ \item QC: Quality Control \item H: Sensible
#'   heat flux [W m-2] \item LE: Latent heat flux [W m-2] \item NEE: Net
#'   ecosystem exchange [umol m-2 s-1] \item IRGA: Infrared Gas Analyzer}.
#'
#' @section References: Mauder, M., Cuntz, M., Drue, C., Graf, A., Rebmann, C.,
#'   Schmid, H.P., Schmidt, M., Steinbrecher, R., 2013. A strategy for quality
#'   and uncertainty assessment of long-term eddy-covariance measurements.
#'   Agric. For. Meteorol. 169, 122-135. doi:10.1016/j.agrformet.2012.09.006
#'
#' @return A data frame. Each column has attributes \code{"varnames"} and
#'   \code{"units"}.
#'
#' @param qc_LE An atomic type containing numeric or NA values. Combination of
#'   all available QC flags for LE.
#' @param qc_H An atomic type containing numeric or NA values. Combination of
#'   all available QC flags for H. Used only if \code{IRGA = "open"}.
#' @param IRGA A character string. Specifies the type of IRGA. Allowed values
#'   are \code{"en_closed"} both for closed and enclosed path systems and
#'   \code{"open"} for open path systems. Can be abbreviated.
#'
#' @seealso \code{\link{combn_QC}} and \code{\link{extract_QC}}.
interdep <- function(qc_LE, qc_H = NULL, IRGA = c("en_closed", "open")) {
  if (!is.atomic(qc_LE) || !is.atomic(qc_H)) {
    stop("'qc_LE' and 'qc_H' must be an atomic type")
  }
  IRGA <- match.arg(IRGA)
  if (!is.null(qc_LE) && !is.numeric(qc_LE) && !all(is.na(qc_LE))) {
    stop("'qc_LE' must contain numeric or NA values")
  }
  if (IRGA == "open") {
    if (!is.null(qc_H) && !is.numeric(qc_H) && !all(is.na(qc_H))) {
      stop("'qc_H' must contain numeric or NA values")
    }
  }
  len <- length(qc_LE)
  if (IRGA == "open" && len != length(qc_H)) {
    stop("length(qc_LE) and length(qc_H) must be equal")
  }
  # qc_H is influencing variable only for open path system
  nout <- c("qc_H_interdep", "qc_LE_interdep", "qc_NEE_interdep")
  out <- data.frame(rep(NA, len), rep(NA, len), rep(NA, len))
  names(out) <- nout
  for (i in nout) {
    attributes(out[, i]) <- list(varnames = i, units = "-")
  }
  if (length(qc_LE) == 0) {
    if (IRGA != "open") out$qc_LE_interdep <- NULL
    return(out)
  }
  qc_LE[is.na(qc_LE)] <- 2L
  out$qc_H_interdep[qc_LE <  2] <- 0L
  out$qc_H_interdep[qc_LE >= 2] <- 1L
  if (IRGA == "open") {
    qc_H[is.na(qc_H)] <- 2L
    out$qc_LE_interdep[qc_H <  2] <- 0L
    out$qc_LE_interdep[qc_H >= 2] <- 1L
    out$qc_NEE_interdep[qc_LE <  2 & qc_H <  2] <- 0L
    out$qc_NEE_interdep[qc_LE >= 2 | qc_H >= 2] <- 1L
  } else {
    out$qc_LE_interdep <- NULL
    out$qc_NEE_interdep[qc_LE <  2] <- 0L
    out$qc_NEE_interdep[qc_LE >= 2] <- 1L
  }
  return(out)
}

#' Apply despiking to a given subset
#'
#' This is a low level function not intended to be used on its own. It is
#' utilized by \code{\link{despikeLF}} that should be used instead.
#'
#' @param xBlock1 The double-differenced \code{var} time series with appropriate
#'   block size.
#' @param xBlock2 Serves for computing despiking threshold and can be from the
#'   same block as \code{xBlock1} (default) or from the whole year (more
#'   conservative when less data available).
#' @param refBlock1 \code{var} values from appropriate block for finding
#'   false-flagged spikes by comparison with scaled median absolute deviation.
#' @param refBlock2 \code{var} values for computing median and mad from the same
#'   block as \code{refBlock1} (default) or whole year (more conservative when
#'   less data available).
#' @param z A numeric value. \eqn{MAD} scale factor.
#' @param c A numeric value. \code{\link{mad}} scale factor. Default is \code{3
#'   * \link{mad} constant} (\code{i.e. 3 * 1.4826 = 4.4478}).
#'
#' @return A logical vector. \code{TRUE} values flag spikes.
#'
#' @seealso use \code{\link{despikeLF}} instead.
desp <- function(xBlock1, xBlock2 = xBlock1, refBlock1,
                 refBlock2 = refBlock1, z, c) {
  med_block <- median(xBlock2, na.rm = TRUE)
  bracket <- z * median(abs(xBlock2 - med_block), na.rm = TRUE) / 0.6745
  med_ref <- median(refBlock2, na.rm = TRUE)
  mad_ref <- mad(refBlock2, constant = c, na.rm = TRUE)
  out <- list(
    spike =
      (xBlock1 < (med_block - bracket) | xBlock1 > (med_block + bracket)) &
      (refBlock1 > (med_ref + mad_ref) | refBlock1 < (med_ref - mad_ref)),
    spike_all =
      (xBlock1 < (med_block - bracket) | xBlock1 > (med_block + bracket)),
    stats = data.frame(med_block = med_block, scaled_MAD = bracket,
                       med_ref = med_ref, mad_ref = mad_ref)
  )
  is.na(out$spike) <- is.na(xBlock1) # cannot check for spikes if d[i] missing
  # this happens for first and last value; has to be treated because of "&"
  return(out)
}

#' Apply despiking to all data blocks
#'
#' This is a low level function not intended to be used on its own. It is
#' utilized by \code{\link{despikeLF}} that should be used instead.
#'
#' @param SD_sub A data frame prepared by \code{\link{despikeLF}} with expected
#'   columns Index, Date, timestamp, var, Spike and Light. This is a subset of
#'   data (\code{x}) provided to \code{\link{despikeLF}} containing only the
#'   data of good quality.
#' @param date An unsubsetted vector of class \code{"Date"} extracted from data
#'   frame \code{x} fed to \code{\link{despikeLF}}.
#' @param nVals A numeric value. Number of values within 13-day blocks required
#'   to obtain robust statistics.
#' @param z A numeric value. \eqn{MAD} scale factor.
#' @param c A numeric value. \code{\link{mad}} scale factor. Default is \code{3
#'   * \link{mad} constant} (\code{i.e. 3 * 1.4826 = 4.4478}).
#' @param plot A logical value. If \code{TRUE}, list of \code{\link{ggplot}}
#'   objects visualizing the spikes is also produced.
#'
#' @return If \code{plot = FALSE}, an updated data frame \code{SD_sub} with
#'   identified spikes in column Spike and with three new columns var_minus,
#'   var_plus and diff. If \code{plot = TRUE}, a list with elements \code{SD} (a
#'   data frame identical to the one produced if \code{plot = FALSE}) and
#'   \code{plots} containing a list of \code{ggplot} objects.
desp_loop <- function(SD_sub, date, nVals, z, c, plot = FALSE) {
  SD_sub$var_minus <- c(NA, SD_sub$var[-nrow(SD_sub)])
  SD_sub$var_plus <- c(SD_sub$var[-1], NA)
  SD_sub$diff <- with(SD_sub, (var - var_minus) - (var_plus - var))
  if (sum(!is.na(SD_sub$diff)) <= nVals) {
    stop("Number of values in subset is below nVals")
  }
  SD_Date <- date[1]
  if (plot) {
    i <- 1L
    plots <- list()
  }
  while (SD_Date <= date[length(date)]) {
    # 13 day block filter
    block <- SD_sub$Date >= SD_Date & SD_sub$Date < SD_Date + 13
    if (sum(block, na.rm = TRUE) > nVals) {
      desp_res <- desp(xBlock1 = SD_sub$diff[block],
                       refBlock1 = SD_sub$var[block], z = z, c = c)
      SD_sub$Spike[block] <- desp_res$spike
    } else if (sum(block, na.rm = TRUE) == 0) {
      SD_Date <- SD_Date + 13
      next
    } else {
      desp_res <- desp(xBlock1 = SD_sub$diff[block],
                       xBlock2 = SD_sub$diff,
                       refBlock1 = SD_sub$var[block],
                       refBlock2 = SD_sub$var, z = z, c = c)
      SD_sub$Spike[block] <- desp_res$spike
    }
    if (plot) {
      x <- na.omit(SD_sub[block, c("timestamp", "diff", "var")])
      x <- reshape2::melt(x, id = "timestamp")
      ts_range <- range(SD_sub[block, "timestamp"])
      y_range_d <- with(desp_res$stats, med_block + c(-scaled_MAD, scaled_MAD))
      y_range_f <- with(desp_res$stats, med_ref + c(-mad_ref, mad_ref))
      d <- data.frame(variable = levels(x$variable),
                      yintercept = c(desp_res$stats$med_block,
                                     desp_res$stats$med_ref),
                      xmin = rep(ts_range[1], 2),
                      xmax = rep(ts_range[2], 2),
                      ymin = c(y_range_d[1], y_range_f[1]),
                      ymax = c(y_range_d[2], y_range_f[2]))
      xs1 <- SD_sub[block, ][desp_res$spike_all, c("timestamp", "diff")]
      xs2 <- SD_sub[block, ][desp_res$spike, c("timestamp", "var")]
      xs <- na.omit(reshape2::melt(merge(xs1, xs2, all = TRUE),
                                   id = "timestamp"))
      plots[[i]] <- ggplot2::ggplot(x, ggplot2::aes(timestamp, value)) +
        ggplot2::geom_point() + ggplot2::geom_line() +
        ggplot2::facet_grid(variable ~ ., scales = "free_y") +
        ggplot2::ggtitle(paste(SD_Date, "-", SD_Date + 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::geom_rect(data = d, ggplot2::aes(xmin = xmin, xmax = xmax,
                                                  ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, alpha = 1/5) +
        ggplot2::geom_hline(data = d, ggplot2::aes(yintercept = yintercept)) +
        ggplot2::geom_point(data = xs, ggplot2::aes(x = timestamp, y = value,
                                                    colour = variable))
      names(plots)[i] <- paste(SD_Date, "-", SD_Date + 12)
      i <- i + 1L
    }
    SD_Date <- SD_Date + 13
  }
  if (plot) return(list(SD = SD_sub, plots = plots))
  return(SD_sub)
}

#' Low Frequency Data Despiking
#'
#' Scaled median absolute deviation from the median is applied to
#' double-differenced time series to identify outliers.
#'
#' Low Frequency Data Despiking is not an additive quality control (QC) test.
#' \code{despikeLF} follows the QC scheme using QC flag range 0 - 2.
#' \code{varnames} attribute of returned vector should be chosen to follow the
#' 'Naming Strategy' described in \code{\link{extract_QC}}, i.e. to be
#' distinguished by suffix \code{"_spikesLF"}.
#'
#' The data frame \code{x} is expected to have certain properties. It is
#' required that it contains column named \code{"timestamp"} of class
#' \code{"POSIXt"} with regular sequence of date-time values, typically with
#' (half-)hourly frequency. Any missing values in \code{"timestamp"} are not
#' allowed. Thus, if no records exist for given date-time value, it still has to
#' be included. It also has to contain required (depends on the argument values)
#' column names. If QC flags are not available for \code{var}, \code{qc_flag}
#' still has to be included in \code{x} as a named column with all values set to
#' \code{0} (i.e. all values will be checked for outliers).
#'
#' Only non-missing \code{var} values with corresponding \code{qc_flag} values
#' below \code{2} are used to detect the outliers. Missing \code{var} values or
#' those with assigned flag \code{2} or \code{NA} are not checked and marked by
#' \code{NA} flag in the output. Thus \code{NA} values of \code{despikeLF}
#' should be considered as not checked records and therefore interpreted as
#' \code{0} flag within the \code{0 - 2} quality control scheme.
#'
#' \code{var_thr} is intended for exclusion of data clearly outside of
#' theoretically acceptable range for the whole dataset. If \code{var_thr} is
#' specified, \code{var} values below \code{var_thr[1]} and above
#' \code{var_thr[2]} are marked as spikes (flag 2) in the output. Such values
#' are further not used for computing statistics on double-differenced time
#' series.
#'
#' \code{light} and \code{night_thr} are intended to separate data to night and
#' day subsets with different statistical properties. \code{NA}s in
#' \code{x[light]} are thus not allowed due to the subsetting. Despiking is then
#' applied to individual subsets and combined QC flags are returned.
#'
#' Despiking is done within blocks of 13 consecutive days to account for
#' seasonality of measured variable. Within each block, all records are compared
#' with its neighbours and \eqn{d[i]} scores are produced. This is achieved by
#' double-differencing: \deqn{d[i] = (var[i] - var[i-1]) - (var[i+1] - var[i])}
#' In order to obtain maximum amount of \eqn{d[i]} scores, all missing
#' \code{var} values are removed from the block before \eqn{d[i]} scores are
#' produced. \code{var} values are marked as spikes if \eqn{d[i]} is higher
#' (lower) than median of \eqn{d[i]} scores (\eqn{M[d]}) + (-) scaled median
#' absolute deviation: \deqn{d[i] > M[d] + (z * MAD / 0.6745)} \deqn{d[i] < M[d]
#' - (z * MAD / 0.6745)} MAD is defined as: \deqn{MAD = median(abs(d[i] -
#' M[d]))}
#'
#' The algorithm tends to flag also values that are neighbours of spikes. To
#' prevent false flagging, \code{\link{median}} and \code{\link{mad}} of
#' \code{var} values within given block (\eqn{M[var]} and \eqn{mad[var]},
#' respectively) is computed. Values can be marked as spikes only if
#' \deqn{var[i] > M[var] + (c * mad / 1.4826)} or \deqn{var[i] < M[var] - (c *
#' mad / 1.4826)}
#'
#' Number of available double-differenced \code{var} values (\code{nVals}) is
#' checked within each block. If equal or below \code{nVals}, \eqn{d[i]} or
#' \eqn{var[i]} values are checked against the statistics computed using entire
#' dataset to ensure robustness.
#'
#' The whole process is repeated iteratively if \code{iter > 1}. This way new
#' statistics are produced for each iteration after exclusion of already
#' detected outliers and new spikes can be identified.
#'
#' @section Plotting: Plots are produced as a list of \code{ggplot} objects.
#'   Thus they can be assigned to an object and modified as needed before actual
#'   plotting. Each plot consists of two panels. The upper one shows the
#'   double-differenced time series, the bottom one the actual \code{var}
#'   values. Grey bands mark the respective intervals in which \code{var} value
#'   cannot be considered as an outlier. The red points in upper panel show all
#'   points that would be marked as spikes if \code{c = 0}. Only the points
#'   marked by blue color (bottom panel) will be considered spikes. The spike
#'   detection tolerance (width of grey bands) can be modified by scale factors
#'   \code{z} (upper panel) and \code{c} (bottom panel).
#'
#' @section Abbreviations: \itemize{\item QC: Quality Control \item PAR:
#'   Photosynthetic Active Radiation [umol m-2 s-1] \item GR: Global Radiation
#'   [W m-2]}
#'
#' @section References: Mauder, M., Cuntz, M., Drue, C., Graf, A., Rebmann, C.,
#'   Schmid, H.P., Schmidt, M., Steinbrecher, R., 2013. A strategy for quality
#'   and uncertainty assessment of long-term eddy-covariance measurements.
#'   Agric. For. Meteorol. 169, 122-135. doi:10.1016/j.agrformet.2012.09.006
#'
#'   Papale, D., Reichstein, M., Canfora, E., Aubinet, M., Bernhofer, C.,
#'   Longdoz, B., Kutsch, W., Rambal, S., Valentini, R., Vesala, T., Yakir, D.,
#'   2006. Towards a more harmonized processing of eddy covariance CO2 fluxes:
#'   algorithms and uncertainty estimation. Biogeosciences Discuss. 3, 961-992.
#'   doi:10.5194/bgd-3-961-2006
#'
#'   Sachs, L., 1996. Angewandte Statistik: Anwendung Statistischer Methoden,
#'   Springer, Berlin.
#'
#' @return If \code{plot = FALSE}, an integer vector with attributes
#'   \code{"varnames"} and \code{"units"}. If \code{plot = TRUE}, a list with
#'   elements \code{SD} and \code{plots}. \code{SD} contains identical output as
#'   produced when \code{plot = FALSE}, \code{plots} contains list of
#'   \code{ggplot} objects for respective iteration, \code{light} subset and
#'   13-day period.
#'
#'   Side effect: the counts of spikes detected in each iteration are printed to
#'   console.
#'
#' @param x A data frame with column names representing required variables. See
#'   'Details' below.
#' @param var A character string. Specifies the variable name in \code{x} with
#'   values to be despiked.
#' @param qc_flag A character string. Specifies the column name in \code{x} with
#'   \code{var} related quality control flag.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#' @param var_thr A numeric vector with 2 non-missing values. Specifies fixed
#'   thresholds for \code{var} values. Values outside this range will be flagged
#'   as spikes (flag 2). If \code{var_thr = NULL}, thresholds are not applied.
#' @param iter An integer value. Defines number of despiking iterations.
#' @param plot A logical value. If \code{TRUE}, list of \code{\link{ggplot}}
#'   objects visualizing the spikes is also produced.
#' @param light A character string. Selects preferred variable for incoming
#'   light intensity. \code{"PAR"} or \code{"GR"} is allowed. Can be
#'   abbreviated. If \code{light = NULL}, \code{var} values are not separated to
#'   night/day subsets and \code{night_thr} is not used.
#' @param night_thr A numeric value that defines the threshold  between night
#'   (for \code{light} values equal or lower than \code{night_thr}) and day (for
#'   \code{light} values higher than \code{night_thr}) for incoming light.
#' @param nVals A numeric value. Number of values within 13-day blocks required
#'   to obtain robust statistics.
#' @param z A numeric value. \eqn{MAD} scale factor.
#' @param c A numeric value. \code{\link{mad}} scale factor. Default is \code{3
#'   * \link{mad} constant} (\code{i.e. 3 * 1.4826 = 4.4478}).
#'
#' @seealso \code{\link{combn_QC}}, \code{\link{extract_QC}},
#'   \code{\link{median}} and \code{\link{mad}}.
despikeLF <- function(x, var, qc_flag, name_out = "-", var_thr = NULL,
                      iter = 10, plot = FALSE, light = c("PAR", "GR"),
                      night_thr = 10, nVals = 50, z = 7, c = 4.4478) {
  x_names <- colnames(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' has to be of class data.frame with colnames")
  }
  if (any(!sapply(list(var, qc_flag, name_out), is.character))) {
    stop("'var', 'qc_flag', 'name_out' has to be of class character")
  }
  var <- var[1]
  qc_flag <- qc_flag[1]
  req_vars <- c("timestamp", var, qc_flag)
  if (!is.null(light)) {
    light <- match.arg(light)
    req_vars <- c(req_vars, light)
    if (!is.numeric(night_thr) || length(night_thr) != 1) {
      stop("'night_thr' must be numeric vector of length 1")
    }
  }
  if (!all(req_vars %in% x_names)) {
    stop(paste("missing", paste0(req_vars[!(req_vars %in% x_names)],
                                 collapse = ", ")))
  }
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (any(is.na(x$timestamp))) stop("NAs in 'x$timestamp' not allowed")
  if (any(diff(as.numeric(x$timestamp)) !=
          mean(diff(as.numeric(x$timestamp))))) {
    stop("timestamp does not form regular sequence")
  }
  if (iter[1] < 1) stop("set iter = 1 or higher")
  date <- as.Date(x$timestamp)
  vals <- x[, var]
  qc_flag <- x[, qc_flag]
  # NA qc is interpreted as flag 2
  qc_flag[is.na(qc_flag)] <- 2L
  if (!is.null(light)) {
    sun <- x[light]
    if (anyNA(sun)) stop(paste0("NAs in x['", light, "'] not allowed"))
  }
  # Filter for used data (qc not flag 2 or NA and var is not NA)
  use <- qc_flag < 2 & !is.na(vals)
  # Introduce Spike flag, set a fixed threshold and update filter
  out <- rep(NA, nrow(x)) # Flag NA: if not changed it means not tested
  if (!is.null(var_thr)) {
    if (!is.numeric(var_thr) || length(var_thr) != 2 || anyNA(var_thr)) {
      stop("'var_thr' must be numeric vector with 2 non-missing values")
    }
    if (var_thr[1] > var_thr[2]) {
      stop("'var_thr[1]' cannot be higher than 'var_thr[2]'")
    }
    out[((vals < var_thr[1]) | (vals > var_thr[2])) & use] <- 2L
    use <- use & is.na(out) # Flag NA at this point means eligible for despiking
  }
  # Spike Detection (Papale et al. 2006)
  # Create data frame with dates, vals & sun intensity values
  SD_df <- data.frame(Index = seq_len(nrow(x)), Date = date,
                      timestamp = x$timestamp, var = vals, Spike = out)
  if (!is.null(light)) {
    SD_df$Light <- sun[, 1]
  }
  if (plot) plots <- list()
  for (i in seq_len(iter)) {
    # Filter out all low quality data and create continuous NEE time series
    # SD_df after filtering keeps only records to be tested
    # On first iteration SD_df$Spike contains only NAs (vals not checked yet)
    # During following iterations SD_df$Spike:
    #  - has 2 (light = NULL) or 4 (night/day) NAs (boundaries of subsets)
    #  - otherwise equals 0 (spikes so far not detected)
    # SD_df is used to merge results from SD_l after despiking
    # SD_df$Spike is a subset of out (out needs to be matched by SD_df$Index)
    # length(SD_df$Spike) decreases with iterations
    SD_df <- SD_df[use, ]
    SD_l <- list(all = SD_df)
    if (!is.null(light)) {
      night <- SD_l$all$Light <= night_thr # filter to distinguish night/day
      SD_l <- list(night = SD_l$all[night, ], day = SD_l$all[!night, ])
    }
    # SD_l data frames have 3 more columns after despiking
    if (plot) {
      temp <- lapply(SD_l, desp_loop, date, nVals, z, c, plot)
      SD_l <- lapply(temp, "[[", "SD") # overwritten every iteration
      plots[[i]] <- lapply(temp, "[[", "plots") # all iteration results kept
      names(plots)[i] <- paste("iter", i)
    } else {
      SD_l <- lapply(SD_l, desp_loop, date, nVals, z, c, plot)
    }
    SD_l <- lapply(SD_l, function(x) {
      x$Spike[x$Spike == TRUE] <- 2L
      return(x)
    })
    # Export results from spike detection list into the data frame
    if (!is.null(light)) {
      SD_df$Spike[match(SD_l$night$Index, SD_df$Index)] <- SD_l$night$Spike
      SD_df$Spike[match(SD_l$day$Index, SD_df$Index)] <- SD_l$day$Spike
    } else SD_df$Spike <- SD_l$all$Spike
    # Export the results into the output vector
    out[SD_df$Index] <- SD_df$Spike
    # Count the number of detected spikes in this iteration and report them
    ns <- sum(SD_df$Spike == 2, na.rm = TRUE)
    cat(paste0("iter ", i, ": ", ns, "\n"))
    if (!ns && i < iter) {
      cat("Further iterations omitted\n")
      break
    }
    # Update use filter - it has to fit the already subsetted SD_df
    # It has different length for each iteration
    use <- SD_df$Spike == 0 | is.na(SD_df$Spike)
  }
  if (plot) return(list(SD = out, plots = plots))
  attributes(out) <- list(varnames = name_out, units = "-")
  return(out)
}

#' Apply Fetch Filter
#'
#' \code{fetch_filter} flags all halfhours that have longer fetch distance (of
#' given percentage of contribution to the flux) than the user defined boundary
#' of the region of interest (ROI).
#'
#' Fetch distance is used together with wind direction information to identify
#' the cases when fetch reached beyond ROI.
#'
#' The spatial extent of the studied ecosystem (ROI) is specified by its
#' \code{ROI_boundary} that describes the distance from eddy covariance tower to
#' the edge of the studied ecosystem. \code{ROI_boundary} has following
#' properties: \itemize{ \item the number of circular sectors is the same as the
#' number of provided distances; \item the angular resolution of the ROI
#' boundary can be computed as 360 degrees / number of angular sectors; \item
#' the ROI boundary distances are assigned to the centers of their respective
#' circular sectors with first sector centered on 0 degrees.}
#'
#' Example: \code{ROI_boundary} specified as c(150, 200, 250, 300) has following
#' properties: \itemize{ \item 4 circular sectors with 90° angular resolution;
#' \item ROI boundary is specified for the whole first sector (315°, 45°] at the
#' distance 150 m from tower (center of the sector is 0°); \item boundary of the
#' second sector (45°, 135°] is at the distance 200 m; \item third sector (135°,
#' 225°] is at the distance 250 m; \item fourth sector (225°, 315°] is at the
#' distance 300 m.}
#'
#' @return An integer vector with attributes \code{"varnames"} and
#'   \code{"units"}.
#'
#' @param x A data frame with column names representing required variables.
#' @param fetch_name A character string. Specifies the column name in \code{x}
#'   with fetch distance values.
#' @param wd_name A character string. Specifies the column name in \code{x} with
#'   wind direction values.
#' @param ROI_boundary A numeric vector. Represents the boundary of region of
#'   interest.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#'
#' @seealso \code{\link{combn_QC}} and \code{\link{extract_QC}}.
#'
#' @examples
#' set.seed(20)
#' (xx <- data.frame(x_70perc = sample(1:1000, 10), WD = sample(0:360, 10)))
#' fetch_filter(xx, "x_70perc", "WD", 300, "qc_ALL_fetch70")
fetch_filter <- function(x, fetch_name, wd_name, ROI_boundary, name_out = "-") {
  x_names <- colnames(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' has to be of class data.frame with colnames")
  }
  if (any(!sapply(list(fetch_name, wd_name, name_out), is.character))) {
    stop("'fetch_name', 'wd_name' and 'name_out' has to be of class character")
  }
  if (!is.numeric(ROI_boundary)) {
    stop("'x' has to be of class numeric")
  }
  sector_span <- 360 / length(ROI_boundary)
  x$sector <- round(x[, wd_name] / sector_span) + 1
  x$sector[x$sector == (length(ROI_boundary) + 1)] <- 1
  out <- rep(NA, nrow(x))
  out[x[, fetch_name] <= ROI_boundary[x$sector]] <- 0L
  out[x[, fetch_name] >  ROI_boundary[x$sector]] <- 2L
  attributes(out) <- list(varnames = name_out, units = "-")
  return(out)
}

#' Exclude values
#'
#' Interactive plots are used for identification and flagging of data for
#' exclusion based on the visual inspection.
#'
#' Five options are available during the interactive session. \enumerate{ \item
#' flag values: select interval of values to exclude or use double-click to flag
#' single value \item undo last: allows to change the user flagging input back
#' to the state before last flagging \item refresh plots: apply current user
#' flagging input to the plots. It removes excluded points and affects y-axis
#' range or it can show again excluded points if option 2 (undo last) was
#' applied before. \item next plot \item jump to plot: any plot within the
#' existing range (see as a plot title) can be selected }
#'
#' The interactive session will be finished succesfully when option 4 (next
#' plot) is executed while at the last plot.
#'
#' @param x A numeric vector with values to inspect.
#' @param qc_x A numeric vector in the range \code{0 - 2} providing quality
#'   control information about \code{x}.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#' @param win_size An integer. Number of \code{x} values displayed per plot.
#' @param other1 A numeric vector of the same length as \code{x} with the first additional variable to be plotted
#' @param other2 A numeric vector of the same length as \code{x} with the second additional variable to be plotted
#' @param other3 A numeric vector of the same length as \code{x} with the third additional variable to be plotted
#' @param col1 A character assignig a colour for \code{other1}
#' @param col2 A character assignig a colour for \code{other2}
#' @param col3 A character assignig a colour for \code{other3}
#'
#' @return An integer vector with attributes \code{"varnames"} and
#'   \code{"units"}.
exclude<-function (x, qc_x = NULL, name_out = "-", win_size = 672, other1 = NULL, other2 = NULL, other3 = NULL, col1 = NULL, col2 = NULL, col3 = NULL) 
{
  if(!exists("other1")) other1 = NA
  if(!exists("other2")) other2 = NA
  if(!exists("other3")) other3 = NA
  if(!exists("col1")) col1 = "black"
  if(!exists("col2")) col2 = "black"
  if(!exists("col3")) col3 = "black"
  
  len <- length(x)
  if (!is.null(qc_x)) {
    if (len != length(qc_x)) 
      stop("'qc_x' must be of same lenght as 'x'")
    x <- ifelse(qc_x == 2, NA, x)
  }
  if (!is.atomic(name_out) || length(name_out) != 1) {
    stop("atomic type 'name_out' must have length 1")
  }
  name_out <- if (name_out %in% c("", NA)) 
    "-"
  else as.character(name_out)
  x_old <- x
  out <- rep(0L, len)
  out_old <- out
  n_iter <- ceiling(len/win_size)
  range <- 1:win_size
  sel_range <- numeric()
  read_opt <- function(msg) {
    opt <- readline(prompt = msg)
    if (!grepl("^[1-5]$", opt)) 
      return(read_opt(msg))
    return(as.integer(opt))
  }
  read_plot_num <- function() {
    n <- readline(prompt = "\nPlot number: \n")
    if (!grepl("^[[:digit:]]+$", n)) 
      return(read_plot_num())
    n <- as.integer(n)
    if (n < 1 | n > n_iter) 
      return(read_plot_num())
    return(n)
  }
  i <- 1
  while (i <= n_iter) {
    
    if (all(is.na(x[range]))){ 
      ylim =  c(0, 0)
    }else{
      ylim = range(c(x[range],other1[range],other2[range],other3[range]), na.rm = TRUE)
    }
    
    plot(range, x[range], type = "o", pch = 19, cex = 0.5, 
         ylim = ylim , main = paste0("Plot ", i, "/", n_iter), ylab = "x", xlab = "Index")
    
    lines(other1,col=col1, type = "o", pch = 19, cex = 0.2)
    lines(other2,col=col2, type = "o", pch = 19, cex = 0.2)
    lines(other2,col=col2, type = "o", pch = 19, cex = 0.2)
    
    filter <- as.logical(out[range])
    lines(range[filter], x[range[filter]], col = "red", 
          type = "o", pch = 19, cex = 0.5)
    msg <- paste0("\n", paste0(rep("-", 14), 
                               collapse = ""), "\nChoose option:\n1. flag values\n2. undo last\n", 
                  "3. refresh plots\n4. next plot\n5. jump to plot ...\n\n")
    repeat {
      opt <- read_opt(msg)
      if (opt == 1) {
        sel <- numeric()
        for (j in 1:2) {
          sel[j] <- identify(x, n = 1, plot = FALSE, 
                             tolerance = 0.25)
          points(sel[j], x[sel[j]], col = "red", 
                 pch = 19, cex = 0.5)
        }
        sel_range <- sel[1]:sel[2]
        lines(sel_range, x[sel_range], col = "red", 
              type = "o", pch = 19, cex = 0.5)
        out_old <- out
        x_old <- x
        out[sel_range] <- 2L
      }
      if (opt == 2) {
        out <- out_old
        x <- x_old
        lines(sel_range, x[sel_range], type = "o", 
              pch = 19, cex = 0.5)
      }
      if (opt == 3) {
        x <- ifelse(out == 2, NA, x)
        plot(range, x[range], type = "o", pch = 19, 
             cex = 0.5, ylim = if (all(is.na(x[range]))) 
               c(0, 0)
             else range(x[range], na.rm = TRUE), main = paste0("Plot ", 
                                                               i, "/", n_iter), ylab = "x", 
             xlab = "Index")
      }
      if (opt == 4) {
        break
      }
      if (opt == 5) {
        n <- read_plot_num()
        i <- n - 1
        break
      }
    }
    i <- i + 1
    range <- ((i - 1) * win_size + 1):(i * win_size)
  }
  attributes(out) <- list(varnames = name_out, units = "-")
  return(out)
}
