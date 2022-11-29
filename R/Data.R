#' used within extract_QC() to link column names with SA and SAGA names
#' @keywords internal
coded_vars <- data.frame(
  QC_suffix =
    c("spikesHF", "ampresHF", "dropoutHF", "abslimHF", "skewkurtHF",
      "skewkurtSF", "discontHF", "discontSF"),
  EddyPro_name =
    c("spikes_hf", "amplitude_resolution_hf", "drop_out_hf",
      "absolute_limits_hf", "skewness_kurtosis_hf", "skewness_kurtosis_sf",
      "discontinuities_hf", "discontinuities_sf"),
  name_out_SA =
    c("qc_SA_spikesHF", "qc_SA_ampresHF", "qc_SA_dropoutHF", "qc_SA_abslimHF",
      "qc_SA_skewkurtHF", "qc_SA_skewkurtSF", "qc_SA_discontHF",
      "qc_SA_discontSF"),
  name_out_SAGA =
    c("qc_SAGA_spikesHF", "qc_SAGA_ampresHF", "qc_SAGA_dropoutHF",
      "qc_SAGA_abslimHF", "qc_SAGA_skewkurtHF", "qc_SAGA_skewkurtSF",
      "qc_SAGA_discontHF", "qc_SAGA_discontSF")
)

