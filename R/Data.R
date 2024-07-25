#' Coded Variables
#'
#' Used within \code{\link{extract_QC}} to link names of filters (QC suffixes)
#' with EddyPro column names and relevant SA, GA, SAGA and ALL output names.
#' @keywords internal
#' @noRd
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


