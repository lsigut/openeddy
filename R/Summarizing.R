# x-data frame, format-format arg of strftime, breaks-breaks arg of cut.POSIXt
# ...-further arguments passed to or used by methods
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

# x: data frame, format: format arg of strftime, breaks: breaks arg of cut.POSIXt
# agg_per: char string describing integration period in units
# quant+power+carbon: column names of 'x' - checked against names(x).
agg_sum <- function(x, format, breaks = NULL, agg_per = NULL,
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
        " -> ", paste(energy_units, agg_per), "):\n\n",
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
        " -> ", paste(energy_units, agg_per), "):\n\n",
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
        " -> ", paste(carbon_units, agg_per), "):\n\n",
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
  openeddy::units(out)[-1] <- paste(openeddy::units(out)[-1], agg_per)
  names(out) <- c("Intervals", paste0(names(out[-1]), "_sum"))
  return(out)
}

# x-data frame, format-format arg of strftime, breaks-breaks arg of cut.POSIXt
# quant, power and carbon applied only if names available
# dots arg not making sense here
# first part - all columns have to fit to fall columns
# second part - GPP has to fit Reco columns, NEE_orig needed for residuals

# value - NULL if required columns not recognized
agg_fsd <- function(x, format, breaks = NULL, agg_per = NULL,
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
  for (i in seq_along(fsd)) fsd[x[, fqc_names[i]] != 0, i] <- NA

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
        " -> ", paste(energy_units, agg_per), "):\n\n",
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
        " -> ", paste(energy_units, agg_per), "):\n\n",
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
        " -> ", paste(carbon_units, agg_per), "):\n\n",
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
  openeddy::units(res_sum)[-1] <- paste(openeddy::units(res_sum)[-1], agg_per)

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

# x-data frame, format-format arg of strftime, breaks-breaks arg of cut.POSIXt
# quant, power and carbon applied only if names available
# dots arg not making sense here
# first part - all columns have to fit to fall columns
# second part - GPP has to fit Reco columns, NEE_orig needed for residuals
agg_DT_SD <- function(x, format, breaks = NULL, agg_per = NULL,
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
  for (i in seq_along(Reco_SD)) Reco_SD[x[, fqc_names[i]] != 0, i] <- NA
  for (i in seq_along(GPP_SD)) GPP_SD[x[, fqc_names[i]] != 0, i] <- NA

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
        " -> ", paste(carbon_units, agg_per), "):\n\n",
        paste(carbon, collapse = ", "),
        "\n-------------------------------------------------------\n", sep = "")
  }
  openeddy::units(res_sum[carbon]) <- rep(carbon_units, ncol(res_sum[carbon]))
  if (length(carbon) == 0)
    cat("No variables available for conversion\n")

  names(res_mean)[-1] <- paste0(names(res_mean[-1]), "_mean")
  names(res_sum)[-1] <- paste0(names(res_sum[-1]), "_sum")
  openeddy::units(res_sum)[-1] <- paste(openeddy::units(res_sum)[-1], agg_per)

  out <- list(mean = res_mean, sum = res_sum)
  return(out)
}

