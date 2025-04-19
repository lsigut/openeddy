#' Coded Variables
#'
#' Used within [extract_QC()] to link names of filters (QC suffixes)
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

#' Eddy covariance data
#'
#' A two week subset of eddy covariance and related meteorological data from the
#' mixed floodplain forest ecosystem station in Lanzhot, Czechia (CZ-Lnz).
#'
#' @format A data frame with 672 rows and 36 columns:
#' \describe{
#'   \item{timestamp}{date-time information in POSIXct format}
#'   \item{GR}{globar radiation}
#'   \item{PAR}{photosynthetically active radiation}
#'   \item{Rn}{net radiation}
#'   \item{Tair}{air temperature}
#'   \item{Tsoil}{soil temperature}
#'   \item{RH}{relative humidity}
#'   \item{VPD}{vapor pressure deficit}
#'   \item{SWC}{soil water content}
#'   \item{P}{precipitation}
#'   \item{G}{soil heat flux}
#'   \item{qc_G}{quality control information related to G}
#'   \item{Tau}{momentum flux}
#'   \item{qc_Tau_SSITC}{steady-state and integral turbulence characteristics
#'   QC information related to Tau flux}
#'   \item{qc_Tau_forGF}{QC information combined using all relevant QC filters
#'   for Tau flux}
#'   \item{H}{sensible heat flux}
#'   \item{qc_H_SSITC}{steady-state and integral turbulence characteristics
#'   QC information related to H flux}
#'   \item{qc_H_forGF}{QC information combined using all relevant QC filters
#'   for H flux}
#'   \item{LE}{latent heat flux}
#'   \item{qc_LE_SSITC}{steady-state and integral turbulence characteristics
#'   QC information related to LE flux}
#'   \item{qc_LE_forGF}{QC information combined using all relevant QC filters
#'   for LE flux}
#'   \item{NEE}{net ecosystem exchange}
#'   \item{qc_NEE_SSITC}{steady-state and integral turbulence characteristics
#'   QC information related to NEE flux}
#'   \item{qc_NEE_forGF}{QC information combined using all relevant QC filters
#'   for NEE flux}
#'   \item{qc_NEE_forGF_UF}{QC information combined using all relevant QC
#'   filters for NEE, including friction velocity (uStar) filtering}
#'   \item{wind_speed}{wind speed}
#'   \item{wind_dir}{wind direction}
#'   \item{ustar}{friction velocity}
#'   \item{H_f}{gap-filled H flux}
#'   \item{LE_f}{gap-filled LE flux}
#'   \item{NEE_uStar_f}{gap-filled NEE after application of uStar filtering}
#'   \item{ET_f}{gap-filled evapotranspiration}
#'   \item{Reco_uStar}{ecosystem respiration derived by nighttime-based
#'   approach}
#'   \item{GPP_uStar_f}{gross primary production derived by nighttime-based
#'   approach}
#'   \item{Reco_DT_uStar}{ecosystem respiration derived by daytime-based
#'   approach}
#'   \item{GPP_DT_uStar}{gross primary production derived by daytime-based
#'   approach}
#' }
"eddy_data"

