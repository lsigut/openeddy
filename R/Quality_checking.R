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
#' For \code{flag = "lower"} \itemize{ \item If \code{x >= thr[1]}, QC flag = 0.
#' \item If \code{x < thr[1] & x >= thr[2]}, QC flag = 1. \item If \code{x <
#' thr[2]}, QC flag = 2.}
#'
#' @return A numeric vector with the same length as \code{x}. Its
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
#' xx$lower  <- apply_thr(xx$var, c(0, -1), "lower", flag = "lower")
#' xx
#' str(xx)
apply_thr <- function(x, thr, name_out, flag = c("higher", "lower")) {
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
  if (flag == "higher") {
    if (thr[1] > thr[2]) stop("'thr[1]' cannot be higher than 'thr[2]'")
    out <- rep(NA, length(x))
    out[x <= thr[1]] <- 0
    out[x >  thr[1]] <- 1
    out[x >  thr[2]] <- 2
  }
  if (flag == "lower") {
    if (thr[1] < thr[2]) stop("'thr[1]' cannot be lower than 'thr[2]'")
    out <- rep(NA, length(x))
    out[x >= thr[1]] <- 0
    out[x <  thr[1]] <- 1
    out[x <  thr[2]] <- 2
  }
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
#' (SA_IRGA) is needed for measurements of LE and FCO2. To confirm the correct
#' performance of SA, all variables \code{"u", "v", "w", "ts"} must have flag 0.
#' In case of SA_IRGA, all variables \code{"u", "v", "w", "ts", "h2o", "co2"}
#' must have flag 0. Results are reported according to the QC scheme using QC
#' flag range 0 - 2.
#'
#' @section Abbreviations: \itemize{ \item QC: Quality Control \item SA: Sonic
#'   Anemometer \item IRGA: InfraRed Gas Analyzer \item Tau: Momentum flux [kg
#'   m-1 s-2] \item H: Sensible heat flux [W m-2] \item LE: Latent heat flux [W
#'   m-2] \item FCO2: CO2 flux [umol m-2 s-1] \item u: Longitudinal wind speed
#'   component [m s-1] \item v: Cross-wind wind speed component [m s-1] \item w:
#'   Vertical wind speed component [m s-1] \item ts: Sonic temperature [degC]
#'   \item h2o: H2O concentration [mmol mol-1] \item co2: CO2 concentration
#'   [umol mol-1]}.
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
#' attr(xx, "units") <- "8u/v/w/ts/h2o/co2"
#' cbind(xx, extract_coded(xx))
extract_coded <- function(x, prefix = "[8]", split = "[/]") {
  if (!is.atomic(x)) stop("'x' must be an atomic type")
  units <- get_units(x)
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
  out$SA[out$SA == 1]           <- 2
  out$SA_IRGA[out$SA_IRGA == 1] <- 2
  for (i in seq_len(ncol(out))) {
    attr(out[, i], "varnames") <- c("SA", "SA_IRGA")[i]
    attr(out[, i], "units") <- "-"
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
#' value sets threshold for flag 2.
#'
#' Check of missing data in averaging period (missfrac) takes into account
#' number of valid records used for given averaging period. This number is
#' further reduced by the sum of count of high frequency data spikes out of
#' variables needed to compute covariance. Covariance pairs are w, u (Tau); w,
#' ts (H); w, h2o (LE) and w, co2 (FCO2).
#'
#' @section Extracted QC Checks: \itemize{ \item Check of plausibility limits
#'   (abslim). Test is not additive. \item Check of high frequency data spike
#'   percentage in averaging period against thresholds (spikesHF). Test is not
#'   additive. \item Check of missing data in averaging period against
#'   thresholds (missfrac). Test is not additive. \item Check of spectral
#'   correction factor against thresholds (scf). Test is not additive. \item
#'   Check of mean unrotated w and w residual after planar fit against
#'   thresholds (wresid). Additive test.}
#'
#' @section Content and format of columns: \itemize{ \item
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
#'   both on SA and IRGA (LE, FCO2) \item qc_Tau, qc_H, qc_LE, qc_FCO2: only
#'   applicable for the respective flux \item qc_ALL: applicable to all fluxes}
#'
#'   \strong{QC suffixes} (specifies which QC check was applied to get this QC
#'   output): \itemize{ \item abslim, spikesHF, missfrac, wresid. See 'Included
#'   QC Checks' above.}

#' @section Abbreviations: \itemize{ \item QC: Quality Control \item SA: Sonic
#'   Anemometer \item IRGA: InfraRed Gas Analyzer \item Tau: Momentum flux [kg
#'   m-1 s-2] \item H: Sensible heat flux [W m-2] \item LE: Latent heat flux [W
#'   m-2] \item FCO2: CO2 flux [umol m-2 s-1] \item u: Longitudinal wind speed
#'   component [m s-1] \item w: Vertical wind speed component [m s-1]  \item ts:
#'   Sonic temperature [degC] \item h2o: H2O concentration [mmol mol-1] \item
#'   co2: CO2 concentration [umol mol-1]}.
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
                       scf = TRUE, wresid = TRUE, prefix = "[8]", split = "[/]",
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
  units <- get_units(x, names = TRUE)
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
      attr(abslim_df[, i], "varnames") <- c(
        "qc_SA_abslim", "qc_SA_IRGA_abslim")[i]
    }
    out$qc_SA_abslim <- abslim_df$SA
    out$qc_SA_IRGA_abslim <- abslim_df$SA_IRGA
  }
  # spikesHF creates composite flags for H, LE and CO2 based on EddyPro
  # spikes_hf column ===========================================================
  if (spikesHF) {
    spike_df <- extract_coded(x$spikes_hf, prefix, split)
    for (i in 1:2) {
      attr(spike_df[, i], "varnames") <- c(
        "qc_SA_spikesHF", "qc_SA_IRGA_spikesHF")[i]
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
      mf_FCO2  <- mf(x[c("w_spikes", "co2_spikes")], ur, mfr)
      out$qc_Tau_missfrac  <- apply_thr(mf_Tau, missfrac_thr, "qc_Tau_missfrac")
      out$qc_H_missfrac    <- apply_thr(mf_H, missfrac_thr, "qc_H_missfrac")
      out$qc_LE_missfrac   <- apply_thr(mf_LE, missfrac_thr, "qc_LE_missfrac")
      out$qc_FCO2_missfrac <- apply_thr(mf_FCO2, missfrac_thr,
                                        "qc_FCO2_missfrac")
    } else {
      warning("'x$file_records' has no non-missing values, skipped missfrac",
              call. = FALSE)
    }
  }
  # scf creates flag for halfhours with excessive spectral correction =======
  if (scf) {
    # Flag according to argument 'scf_thr' (spectral correction factor
    # thresholds): scf > scf_thr[1]: flag = 1, scf > scf_thr[2]: flag = 2
    nin <- c("Tau_scf", "H_scf", "LE_scf", "co2_scf")
    nout <- c("qc_Tau_scf", "qc_H_scf", "qc_LE_scf", "qc_FCO2_scf")
    for (i in seq_along(nout)) {
      out[, nout[i]] <- apply_thr(x[, nin[i]], scf_thr, nout[i])
    }
  }
  # wresid creates overall flag for halfhours with probable advection =======
  if (wresid) {
    # Flag correction according to residual absolute w after planar fit
    # abs(w) > 0.10 m s-1: flag incresead by +1, abs(w) > 0.15 m s-1: flag = 2
    rot <- apply_thr(abs(x$w_rot), w_rot_thr, "w_rot")
    # In any case (double or planar fit rotation) w_unrot must be < 0.35 m/s
    unrot <- apply_thr(abs(x$w_unrot), w_unrot_thr, "w_unrot")
    out$qc_ALL_wresid <- combn_QC(data.frame(rot, unrot), c("rot", "unrot"),
                                  "qc_ALL_wresid")
  }
  return(out)
}

#' Flux Interdependency
#'
#' Interdependency of H, LE and FCO2 QC flags due to corrections/conversions.
#'
#' Flux interdependency is an additive QC flag correction. Results follow the QC
#' scheme using QC flag range 0 - 2. Returned data frame follows the 'Naming
#' Strategy' described in \code{\link{extract_QC}}. Returned QC flags have QC
#' suffix interdep.
#'
#' To convert buoyancy flux to sensible heat flux (SND or Schotanus correction),
#' reliable measurements of LE must be available. To correct LE and FCO2
#' measured by open-path IRGA (\code{IRGA = "open"}) for the effects of density
#' fluctuations due to temperature and humidity fluctuations (WPL or Webb
#' correction), reliable measurements of H must be available. To perform WPL
#' correction of FCO2 measured with any kind of IRGA, reliable measurements of
#' LE must be available. Thus following set of rules apply:
#'
#' If \code{IRGA = "en_closed"} \itemize{ \item If \code{qc_LE == 2 |
#' is.na(qc_LE) == TRUE}: qc_H and qc_FCO2 flags are increased by 1.} If
#' \code{IRGA = "open"} \itemize{ \item if \code{qc_LE == 2 | is.na(qc_LE) ==
#' TRUE}: qc_H flags are increased by 1. \item If \code{qc_H == 2 | is.na(qc_H)
#' == TRUE}: qc_LE flags are increased by 1. \item If \code{qc_H == 2 |
#' is.na(qc_H) == TRUE | qc_LE == 2 | is.na(qc_LE) == TRUE}: qc_FCO2 flags are
#' increased by 1.}
#'
#' @section Abbreviations: \itemize{ \item QC: Quality Control \item H: Sensible
#'   heat flux [W m-2] \item LE: Latent heat flux [W m-2] \item FCO2: CO2 flux
#'   [umol m-2 s-1] \item IRGA: InfraRed Gas Analyzer}.
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
  nout <- c("qc_H_interdep", "qc_LE_interdep", "qc_FCO2_interdep")
  out <- data.frame(rep(NA, len), rep(NA, len), rep(NA, len))
  names(out) <- nout
  for (i in nout) {
    attributes(out[, i]) <- list(varnames = i, units = "-")
  }
  if (length(qc_LE) == 0) {
    if (IRGA != "open") out$qc_LE_interdep <- NULL
    return(out)
  }
  qc_LE[is.na(qc_LE)] <- 2
  out$qc_H_interdep[qc_LE <  2] <- 0
  out$qc_H_interdep[qc_LE >= 2] <- 1
  if (IRGA == "open") {
    qc_H[is.na(qc_H)] <- 2
    out$qc_LE_interdep[qc_H <  2] <- 0
    out$qc_LE_interdep[qc_H >= 2] <- 1
    out$qc_FCO2_interdep[qc_LE <  2 & qc_H <  2] <- 0
    out$qc_FCO2_interdep[qc_LE >= 2 | qc_H >= 2] <- 1
  } else {
    out$qc_LE_interdep <- NULL
    out$qc_FCO2_interdep[qc_LE <  2] <- 0
    out$qc_FCO2_interdep[qc_LE >= 2] <- 1
  }
  return(out)
}

# despiking function; outputs logical vector; TRUE value flags spike;
# xBlock1 is the double-differenced NEE time series with appropriate block size
# (Papale et al., 2006; see below);
# xBlock2 serves for computing despiking threshold and can be
# from the same block as xBlock1 (default) or from the whole year (more
# conservative when less data available)
# refBlock1 - flux values from appropriate block for finding false-flagged
# spikes by comparison with scaled median absolute deviation
# refBlock2 - flux values for computing median and mad from the same block
# as refBlock1 (default) or whole year (more conservative when less data
# available)
# c - scale factor for mad (default = 3)
# z - represents the threshold value
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

# Subset spike detection loop for 13 day blocks (Papale et al., 2006)
# diff = (NEEi - NEEi-1) - (NEEi+1 - NEEi);
# NEEi = NEE; NEEi-1 = NEE_minus; NEEi+1 = NEE_plus
# ggplotting - plot = TRUE
desp_loop <- function(SD_sub, date, nVals, z, c, plot = FALSE) {
  SD_sub$flux_minus <- c(NA, SD_sub$flux[-nrow(SD_sub)])
  SD_sub$flux_plus <- c(SD_sub$flux[-1], NA)
  SD_sub$diff <- with(SD_sub, (flux - flux_minus) - (flux_plus - flux))
  if (sum(!is.na(SD_sub$diff)) <= nVals) {
    stop("Number of values in subset is below nVals")
  }
  SD_Date <- date[1]
  if (plot) {
    i <- 1
    plots <- list()
  }
  while (SD_Date <= date[length(date)]) {
    # 13 day block filter
    block <- SD_sub$Date >= SD_Date & SD_sub$Date < SD_Date + 13
    if (sum(block, na.rm = TRUE) > nVals) {
      desp_res <- desp(xBlock1 = SD_sub$diff[block],
                       refBlock1 = SD_sub$flux[block], z = z, c = c)
      SD_sub$Spike[block] <- desp_res$spike
    } else if (sum(block, na.rm = TRUE) == 0) {
      SD_Date <- SD_Date + 13
      next
    } else {
      desp_res <- desp(xBlock1 = SD_sub$diff[block],
                       xBlock2 = SD_sub$diff,
                       refBlock1 = SD_sub$flux[block],
                       refBlock2 = SD_sub$flux, z = z, c = c)
      SD_sub$Spike[block] <- desp_res$spike
    }
    if (plot) {
      x <- na.omit(SD_sub[block, c("timestamp", "diff", "flux")])
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
      xs2 <- SD_sub[block, ][desp_res$spike, c("timestamp", "flux")]
      xs <- na.omit(reshape2::melt(merge(xs1, xs2, all = TRUE),
                                   id = "timestamp"))
      plots[[i]] <- ggplot(x, aes(timestamp, value)) +
        geom_point() + geom_line() +
        facet_grid(variable ~ ., scales = "free_y") +
        ggtitle(paste(SD_Date, "-", SD_Date + 12)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_rect(data = d, aes(xmin = xmin, xmax = xmax,
                                ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, alpha = 1/5) +
        geom_hline(data = d, aes(yintercept = yintercept)) +
        geom_point(data = xs, aes(x = timestamp, y = value, colour = variable))
      i <- i + 1
    }
    SD_Date <- SD_Date + 13
  }
  if (plot) return(plots)
  return(SD_sub)
}

#' Low Frequency Data Despiking
#'
#' Scaled median absolute deviation from the median is applied to
#' double-differenced time series to identify outliers.
#'
#' Low Frequency Data Despiking is not an additive QC test. \code{despikeLF}
#' follows the QC scheme using QC flag range 0 - 2. \code{varnames} attribute of
#' returned vector follows the 'Naming Strategy' described in
#' \code{\link{extract_QC}} and is distinguished by suffix spikesLF.
#'
#' The data frame \code{x} is expected to have certain properties. It is
#' required that it contains column named \code{"timestamp"} of class
#' \code{"POSIXt"} with regular sequence of date-time values, typically with
#' (half-)hourly frequency. Any missing values in \code{"timestamp"} are not
#' allowed. Thus, if no records exist for given date-time value, it still has to
#' be included. It also has to contain required (depends on the argument values)
#' column names. If QC flags are not available for given flux, \code{qc_flag}
#' still has to be included in \code{x} as a named column with \code{0} values
#' (i.e. all values will be checked for outliers).
#'
#' Only non-missing \code{flux} values with corresponing \code{qc_flag} values
#' below \code{2} are used to detect the outliers. Missing \code{flux} values or
#' those with assigned flag \code{2} or \code{NA} are not checked and marked by
#' \code{NA} flag in the output.
#'
#' \code{flux_thr} is intended for exclusion of data clearly outside of
#' theoretically acceptable range for the whole dataset. If \code{flux_thr} is
#' specified, \code{flux} values below \code{flux_thr[1]} and above
#' \code{flux_thr[2]} are marked as spikes (flag 2) in the output. Such values
#' are further not used for computing statistics on double-differenced time
#' series.
#'
#' \code{light} and \code{night_thr} are intended to separate data to nighttime
#' and daytime subsets with different statistical properties. Despiking is then
#' applied to individual subsets and combined QC flags are returned.
#'
#' Despiking is done within blocks of 13 consecutive days to account for
#' seasonality of measured \code{flux}. Within each block, all records are
#' compared with its neighbours and \eqn{d[i]} scores are produced. This is
#' achieved by double-differencing: \deqn{d[i] = (flux[i] - flux[i-1]) -
#' (flux[i+1] - flux[i])} In order to obtain maximum amount of \eqn{d[i]}
#' scores, all missing flux values are removed from the block before \eqn{d[i]}
#' scores are produced. \code{flux} values are marked as spikes if \eqn{d[i]} is
#' higher (lower) than median of \eqn{d[i]} scores (\eqn{M[d]}) + (-) scaled
#' median absolute deviation: \deqn{d[i] > M[d] + (z * MAD / 0.6745)} \deqn{d[i]
#' < M[d] - (z * MAD / 0.6745)} MAD is defined as: \deqn{MAD = median(abs(d[i] -
#' M[d]))}
#'
#' The algorithm tends to flag also values that are neighbours of spikes. To
#' prevent false flagging, \code{\link{median}} and \code{\link{mad}} of
#' \code{flux} values within given block (\eqn{M[flux]} and \eqn{mad[flux]},
#' respectively) is computed. Values can be marked as spikes only if
#' \deqn{flux[i] > M[flux] + (c * mad)} or \deqn{flux[i] < M[flux] - (c * mad)}
#'
#' Number of available double-differenced flux values (\code{nVals}) is checked
#' within each block. If equal or below \code{nVals}, \eqn{d[i]} or
#' \eqn{flux[i]} values are checked against the statistics computed using entire
#' dataset to ensure robustness.
#'
#' @section Abbreviations: \itemize{\item QC: Quality Control \item PAR:
#'   Photosynthetic Active Radiation [umol m-2 s-1] \item GR: Global Radiation
#'   [W m-2]}
#'
#' @section References: Sachs, L., 1996. Angewandte Statistik: Anwendung
#'   Statistischer Methoden, Springer, Berlin.
#'
#'   Papale, D., Reichstein, M., Canfora, E., Aubinet, M., Bernhofer, C.,
#'   Longdoz, B., Kutsch, W., Rambal, S., Valentini, R., Vesala, T., Yakir, D.,
#'   2006. Towards a more harmonized processing of eddy covariance CO2 fluxes:
#'   algorithms and uncertainty estimation. Biogeosciences Discuss. 3, 961-992.
#'   doi:10.5194/bgd-3-961-2006
#'
#'   Mauder, M., Cuntz, M., Drue, C., Graf, A., Rebmann, C., Schmid, H.P.,
#'   Schmidt, M., Steinbrecher, R., 2013. A strategy for quality and uncertainty
#'   assessment of long-term eddy-covariance measurements. Agric. For. Meteorol.
#'   169, 122-135. doi:10.1016/j.agrformet.2012.09.006
#'
#' @return If \code{plot = FALSE}, a numeric vector with attributes
#'   \code{"varnames"} and \code{"units"}. If \code{plot = TRUE}, a list of
#'   \code{ggplot} objects.
#'
#' @param x A data frame with column names representing required variables. See
#'   'Details' below.
#' @param flux A character string. Specifies the column name in \code{x} with
#'   flux values.
#' @param qc_flag A character string. Specifies the column name in \code{x} with
#'   \code{flux} related quality control flag.
#' @param name_out A character string providing \code{varnames} attribute value
#'   of the output.
#' @param flux_thr A numeric vector with 2 non-missing values. Specifies fixed
#'   thresholds for \code{flux} column. Values outside this range will be
#'   flagged as spikes (flag 2). If \code{flux_thr = NULL}, thresholds are not
#'   applied.
#' @param plot A logical value. If \code{FALSE} (the default), a numeric vector
#'   identifying spikes is produced. If \code{TRUE}, list of
#'   \code{\link{ggplot}} objects visualizing the spikes is returned instead.
#' @param light A character string. Selects preferred variable for incoming
#'   light intensity. \code{"PAR"} or \code{"GR"} is allowed. Can be
#'   abbreviated. If \code{light = NULL}, \code{flux} values are not separated
#'   to nighttime/daytime subsets and \code{night_thr} is not used.
#' @param night_thr A numeric value that defines the threshold  between night
#'   (for \code{light} values equal or lower than \code{night_thr}) and day (for
#'   \code{light} values higher than \code{night_thr}) for incoming light.
#' @param nVals A numeric value. Number of values withing 13 day blocks required
#'   to obtain robust statistics.
#' @param z A numeric value. \eqn{MAD} scaling factor.
#' @param c A numeric value. \code{\link{mad}} \code{constant}.
#'
#' @seealso \code{\link{combn_QC}}, \code{\link{extract_QC}},
#'   \code{\link{median}} and \code{\link{mad}}.
despikeLF <- function(x, flux, qc_flag, name_out, flux_thr = NULL, plot = FALSE,
                      light = c("PAR", "GR"), night_thr = 0,
                      nVals = 50, z = 7, c = 4.4478) {
  x_names <- colnames(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' has to be of class data.frame with colnames")
  }
  if (any(!sapply(list(flux, qc_flag, name_out), is.character))) {
    stop("'flux', 'qc_flag', 'name_out' has to be of class character")
  }
  flux <- flux[1]
  qc_flag <- qc_flag[1]
  req_vars <- c("timestamp", flux, qc_flag)
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
  date <- as.Date(x$timestamp)
  vals <- x[, flux]
  qc_flag <- x[, qc_flag]
  # NA qc is interpreted as flag 2
  qc_flag[is.na(qc_flag)] <- 2
  if (!is.null(light)) sun <- x[light]
  # Filter for used data (qc below flag 2 and flux is not NA)
  use <- qc_flag < 2 & !is.na(vals)
  # Introduce Spike flag, set a fixed threshold and update filter
  out <- rep(NA, nrow(x))
  if (!is.null(flux_thr)) {
    if (!is.numeric(flux_thr) || length(flux_thr) != 2 || anyNA(flux_thr)) {
      stop("'flux_thr' must be numeric vector with 2 non-missing values")
    }
    if (flux_thr[1] > flux_thr[2]) {
      stop("'flux_thr[1]' cannot be higher than 'flux_thr[2]'")
    }
    out[((vals < flux_thr[1]) | (vals > flux_thr[2])) & use] <- 2
    use <- use & is.na(out)
  }
  # Spike Detection (Papale et al. 2006)
  # Create data frame with dates, vals & sun intensity values
  SD_df <- data.frame(Index = seq_len(nrow(x)), Date = date,
                      timestamp = x$timestamp, flux = vals, Spike = out)
  if (!is.null(light)) {
    SD_df$Light <- sun[, 1]
  }
  # filter out all low quality data and create continuous NEE time series
  SD_df <- SD_df[use, ]
  if (is.null(light)) {
    # Daytime & nighttime spike detection (no subsetting)
    SD_df <- desp_loop(SD_df, date, nVals, z, c, plot)
    if (plot) return(SD_df)
    SD_df$Spike[SD_df$Spike == TRUE] <- 2
  } else {
    # Night-time subset spike detection
    SD_night <- SD_df[SD_df$Light <= night_thr, ] # night-time filter
    SD_night <- desp_loop(SD_night, date, nVals, z, c, plot)
    # Daytime subset spike detection
    SD_day <- SD_df[SD_df$Light > night_thr, ] # daytime filter
    SD_day <- desp_loop(SD_day, date, nVals, z, c, plot)
    if (plot) return(list(nighttime = SD_night, daytime = SD_day))
    # Export the results into the SD dataframe
    SD_night$Spike[SD_night$Spike == TRUE] <- 2
    SD_df$Spike[match(SD_night$Index, SD_df$Index)] <- SD_night$Spike
    # Export the results into the SD dataframe
    SD_day$Spike[SD_day$Spike == TRUE] <- 2
    SD_df$Spike[match(SD_day$Index, SD_df$Index)] <- SD_day$Spike
  }
  # Export all results into the main dataframe
  out[SD_df$Index] <- SD_df$Spike
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
#' @return An atomic type with attributes \code{"varnames"} and \code{"units"}.
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
fetch_filter <- function(x, fetch_name, wd_name, ROI_boundary, name_out) {
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
