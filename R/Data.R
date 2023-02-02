#' Coded Variables
#'
#' Used within \code{\link{extract_QC}} to link names of filters (QC suffixes)
#' with EddyPro column names and relevant SA, GA, SAGA and ALL output names.
#' @keywords internal
coded_vars <- data.frame(
  QC_suffix =
    c("spikesHF", "ampres", "dropout", "abslim", "skewkurt_hf",
      "skewkurt_sf", "discont_hf", "discont_sf", "timelag_hf", "timelag_sf",
      "attangle", "nonsteady"),
  EddyPro_name =
    c("spikes_hf", "amplitude_resolution_hf", "drop_out_hf",
      "absolute_limits_hf", "skewness_kurtosis_hf", "skewness_kurtosis_sf",
      "discontinuities_hf", "discontinuities_sf", "timelag_hf", "timelag_sf",
      "attack_angle_hf", "non_steady_wind_hf"),
  name_out_SA =
    c("qc_SA_spikesHF", "qc_SA_ampres", "qc_SA_dropout", "qc_SA_abslim",
      "qc_SA_skewkurt_hf", "qc_SA_skewkurt_sf", "qc_SA_discont_hf",
      "qc_SA_discont_sf", rep(NA, 4)),
  name_out_GA =
    c(rep(NA, 8), "qc_GA_timelag_hf", "qc_GA_timelag_sf", c(rep(NA, 2))),
  name_out_SAGA =
    c("qc_SAGA_spikesHF", "qc_SAGA_ampres", "qc_SAGA_dropout",
      "qc_SAGA_abslim", "qc_SAGA_skewkurt_hf", "qc_SAGA_skewkurt_sf",
      "qc_SAGA_discont_hf", "qc_SAGA_discont_sf", rep(NA, 4)),
  name_out_ALL =
    c(rep(NA, 10), "qc_ALL_attangle", "qc_ALL_nonsteady")
)

#' Precheck Variables
#'
#' A set of variables typically available in EddyPro full output that can be
#' useful for preliminary check before quality control procedure.
#'
#' @export
precheck_vars <-
  c("u_rot", "v_rot", "w_unrot", "w_rot",
    "sonic_temperature", "max_wind_speed",
    "Tau", "ustar", "H", "LE", "NEE",
    "u_var", "v_var", "w_var", "ts_var", "h2o_var", "co2_var",
    "rand_err_Tau", "rand_err_H", "rand_err_LE", "rand_err_NEE",
    "Tau_scf", "H_scf", "LE_scf", "co2_scf",
    "u_spikes", "v_spikes", "w_spikes", "ts_spikes", "co2_spikes",
    "h2o_spikes",
    "H_strg", "LE_strg", "co2_strg",
    "h2o_v_adv", "co2_v_adv",
    "co2_mixing_ratio", "h2o_mixing_ratio",
    "co2_time_lag", "h2o_time_lag",
    "x_peak", "x_70perc",
    "mean_value_RSSI_LI_7200", "co2_signal_strength_7200_mean",
    "h2o_signal_strength_7200_mean", "flowrate_mean")

#' Quality Control Essential Variables
#'
#' A minimal set of variables useful when working with quality controlled data.
#'
#' @export
essential_vars_QC <- c(
  "timestamp", "GR", "qc_GR", "PAR", "qc_PAR", "Rn", "qc_Rn", "Tair",
  "qc_Tair", "Tsoil", "qc_Tsoil", "RH", "qc_RH", "VPD", "qc_VPD", "SWC",
  "qc_SWC", "P", "qc_P", "G", "qc_G", "Tau", "Tau_orig", "qc_Tau_forGF",
  "qc_Tau_SSITC", "rand_err_Tau", "H", "H_orig", "qc_H_forGF", "qc_H_SSITC",
  "rand_err_H", "LE", "LE_orig", "qc_LE_forGF", "qc_LE_SSITC", "rand_err_LE",
  "NEE", "NEE_orig", "qc_NEE_forGF", "qc_NEE_SSITC", "rand_err_NEE", "H_strg",
  "LE_strg", "co2_strg", "wind_speed",
  "wind_dir", "ustar", "L", "zeta", "model", "x_peak", "x_70perc")