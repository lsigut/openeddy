#' Folder Structure Setup
#'
#' Folder structure recommended for eddy covariance data processing and
#' archiving.
#'
#' `make_paths()` superseded `structure_eddy()` (kept only to assure backward
#' compatibility).
#'
#' The purpose is to standardize the locations for data and metadata used in the
#' proposed workflow (<https://github.com/lsigut/EC_workflow>) and separate
#' them into levels corresponding to the processing stage. The folder structure
#' is not required to successfully apply the workflow but simplifies its use.
#'
#' Data processing stages:
#' * level_1: EddyPro full output files, meteorological data and their merged
#' product as an input for quality checking.
#' * level_2: quality checking results and documentation, and files used as
#' inputs for gap-filling.
#' * level_3: gap-filling and flux partitioning output and its documentation,
#' summary of the computed fluxes and meteorological data including their
#' aggregation and plotting.
#'
#' @param root A character string defining the root of created folder structure.
#' @param create_dirs A logical value. Indicates whether directories should be
#'   created.
#' @param fsep A character. The path separator to use (assumed to be ASCII).
#' @param ... Further arguments to be passed to `dir.create` function.
#'
#' @return A named list with paths to folder structure directories.
#'   Corresponding directories are created as a function side effect if
#'   `create_dirs = TRUE`.
#'
#' @seealso [file.path()]
#'
#' @examples
#' xx <- make_paths()
#' xx
#' xx$input_for_gf
#'
#' @export
make_paths <- function(root = ".", create_dirs = FALSE,
                       fsep = .Platform$file.sep, ...) {
  # With dir.create(recursive = TRUE, ...) all paths not needed to create dirs
  # but needed in order to make the dir accessible with path in the list
  l <- list(
    qc_input_eddypro = file.path(
      root, "level_1", "qc_input_eddypro", fsep = fsep
    ),
    qc_input_meteo = file.path(
      root, "level_1", "qc_input_meteo", fsep = fsep
    ),
    input_for_qc = file.path(
      root, "level_1", "input_for_qc", fsep = fsep
    ),
    quality_checking = file.path(
      root, "level_2", "quality_checking", fsep = fsep
    ),
    precheck = file.path(
      root, "level_2", "quality_checking", "precheck", fsep = fsep
    ),
    wd_dependency = file.path(
      root, "level_2", "quality_checking", "precheck", "wd_dependency",
      fsep = fsep
    ),
    qc_summary = file.path(
      root, "level_2", "quality_checking", "qc_summary", fsep = fsep
    ),
    input_for_gf = file.path(
      root, "level_2", "input_for_gf", fsep = fsep
    ),
    gap_filling = file.path(
      root, "level_3", "gap_filling", fsep = fsep
    ),
    plots = file.path(
      root, "level_3", "gap_filling", "plots", fsep = fsep
    ),
    ustar_filtering = file.path(
      root, "level_3", "gap_filling", "ustar_filtering",
      fsep = fsep
    ),
    summary = file.path(
      root, "level_3", "summary", fsep = fsep
    ),
    png = file.path(
      root, "level_3", "summary", "png", fsep = fsep
    ))
  if (create_dirs) invisible(lapply(l, dir.create, recursive = TRUE, ...))
  return(l)
}

#' @rdname make_paths
#' @export
structure_eddy <- function(root = ".", create_dirs = FALSE,
                           fsep = .Platform$file.sep, ...) {
  # With dir.create(recursive = TRUE, ...) all paths not needed to create dirs
  # but needed in order to make the dir accessible with path in the list
  l <- list(
    Processing_setup = file.path(
      root, "Level 0", "EddyPro setup", fsep = fsep
    ),
    IRGA_setup = file.path(
      root, "Level 0", "IRGA setup", fsep = fsep
    ),
    Raw_data = file.path(
      root, "Level 0", "Raw data", fsep = fsep
    ),
    Logbook = file.path(
      root, "Level 1", "Logbook", fsep = fsep
    ),
    Processing = file.path(
      root, "Level 1", "Post-processing", "EddyProOutput", fsep = fsep
    ),
    Quality_checking = file.path(
      root, "Level 2", "Quality checking", fsep = fsep
    ),
    Precheck = file.path(
      root, "Level 2", "Quality checking", "Precheck", fsep = fsep
    ),
    WD_dependency = file.path(
      root, "Level 2", "Quality checking", "Precheck", "WD_dependency",
      fsep = fsep
    ),
    QC_summary = file.path(
      root, "Level 2", "Quality checking", "QC_summary", fsep = fsep
    ),
    Storage_flux = file.path(
      root, "Level 2", "Storage flux", fsep = fsep
    ),
    Input_for_GF = file.path(
      root, "Level 2", "Input for gap-filling", fsep = fsep
    ),
    Gap_filling = file.path(
      root, "Level 3", "Gap-filling", "REddyProc", fsep = fsep
    ),
    Plots = file.path(
      root, "Level 3", "Gap-filling", "REddyProc", "Plots", fsep = fsep
    ),
    Ustar_filtering = file.path(
      root, "Level 3", "Gap-filling", "REddyProc", "Ustar filtering",
      fsep = fsep
    ),
    Summary = file.path(
      root, "Level 3", "Summary", "REddyProc", fsep = fsep
    ),
    png = file.path(
      root, "Level 3", "Summary", "REddyProc", "png", fsep = fsep
    ))
  if (create_dirs) invisible(lapply(l, dir.create, recursive = TRUE, ...))
  return(l)
}

#' Round Numeric Columns in Data Frame
#'
#' Round the columns of numeric mode type `"double"` to specified (default = 6)
#' significant digits.
#'
#' Actual [signif()] function is used for rounding. Note that other
#' classes might be internally stored as numeric types. Particularly
#' [POSIXct()] class is by default stored as integer (rounding does
#' not apply) but in case of adding (subtracting) double or if displaying
#' fractional seconds, such date-time information will be internally converted
#' to the type double (rounding applies). See examples.
#'
#' @param x A data frame.
#' @param digits An integer. See [signif()] for details.
#'
#' @return A data frame with `varnames` and `units` attributes.
#'
#' @examples
#' set.seed(123)
#' n <- 17520 # number of half-hourly records in one non-leap year
#' tstamp <- seq(c(ISOdate(2021,3,20)), by = "30 mins", length.out = n)
#' x <- data.frame(
#' timestamp = tstamp,
#' H = rf(n, 1, 2, 1),
#' LE = rf(n, 1, 2, 1),
#' qc_flag = sample(c(0:2, NA), n, replace = TRUE)
#' )
#' openeddy::varnames(x) <- c("timestamp", "sensible heat", "latent heat",
#'                            "quality flag")
#' openeddy::units(x) <- c("-", "W m-2", "W m-2", "-")
#' str(x)
#' r <- round_df(x)
#' head(r)
#' str(r) # varnames and units are preserved
#'
#' # Prevent adding double type to POSIXct as it would lead to rounding:
#' y <- x
#' y$timestamp <- y$timestamp - 900 # use 900L instead
#' head(y)
#' class(y$timestamp)
#' is.double(y$timestamp)
#' head(round_df(y))
#'
#' @export
round_df <- function(x, digits = 6) {
  v <- varnames(x)
  u <- units(x)
  col_double <- unlist(lapply(x, is.double))
  rounded <- apply(data.matrix(x[col_double]), 2, signif, digits = digits,
                   simplify = FALSE) # this assures 1 row df is handled well
  x[col_double] <- as.data.frame(rounded)
  varnames(x) <- v
  units(x) <- u
  return(x)
}

#' Find Rows or Columns with Only NAs Over Array Margins
#'
#' `allNA` returns a logical vector or array or list indicating whether
#' there are only `NA` values in selected margins and therefore e.g.
#' statistics like `max` or `min` do not produce useful results.
#' @param x An array, including a matrix.
#' @param margin A vector giving the subscripts which the function will be
#'   applied over. E.g., for a matrix `1` indicates rows, `2`
#'   indicates columns, `c(1, 2)` indicates rows and columns. Where
#'   `x` has named dimnames, it can be a character vector selecting
#'   dimension names.
#' @family NA handlers
#' @seealso [NA] for general information about NAs and
#'   [apply()] for `apply` description.
#' @examples
#' xx <- matrix(1:20, nrow = 4)
#' xx[2, ] <- NA
#' allNA(xx, 2) # All columns have at least one non-missing value
#' allNA(xx, 1) # Second row has all values missing
#' try(apply(xx, 1, max, na.rm = TRUE))
#' ## returns c(17, -Inf, 19, 20) and a warning message
#' ## Skip the allNA row in apply()
#' apply(xx[!allNA(xx, 1), ], 1, max, na.rm = TRUE)
#'
#' @keywords internal
#' @noRd
allNA <- function(x, margin) {
  apply(x, margin, function(x) all(is.na(x)))
}

#' Object Attributes Varnames and Units
#'
#' `varnames` and `units` are useful attributes that can store
#' original variable names (`varnames`) and units of measurement
#' (`units`) of each column in a data frame or of an atomic type. These
#' attributes can be extracted or assigned by following functions.
#'
#' Functions check whether the extracted or assigned attributes contain elements
#' with `NULL`, `NA`, `""` values or if length of each element is
#' higher than `1`. In these cases, such elements are substituted with
#' `"-"`.
#'
#' @return For `varnames` and `units`, a character vector.
#'
#'   For `varnames<-` and `units<-`, the updated object `x`.
#'
#' @param x A data frame or an [atomic] type.
#' @param names A logical value. Applies only in case of data frames. If
#'   `TRUE`, attributes are extracted with corresponding column names.
#' @param value An [atomic] type that represents `varnames` or `units`.
#'   The length must be `1` if `x` is an atomic type or equal to
#'   number of columns in `x` if `x` is a data frame.
#'
#' @seealso [read_eddy()] and [write_eddy()].
#'
#' @examples
#' xx <- data.frame(a = 1, b = 2, c = 3, d = 4)
#' lapply(xx, attr, "units")
#' units(xx, names = TRUE)
#' varnames(xx) <- c("a", "", NA, "d")
#' units(xx) <- 1:4
#' str(xx)
#' units <- units(xx)
#'
#' ## NB: subsetting by rows removes 'varnames' and 'units' attributes
#' str(yy <- xx[1, ])
#' varnames(yy) <- names(yy)
#' units(yy) <- units
#' str(yy)
#' @export
varnames <- function(x, names = FALSE) {
  if (is.data.frame(x)) {
    varnames <- lapply(x, attr, "varnames")
    varnames <- lapply(varnames, function(x) if (
      is.null(x) || x %in% c("", NA) || (length(x) != 1))
      "-" else as.character(x))
    varnames <- unlist(varnames, use.names = names)
    return(varnames)
  } else if (is.atomic(x)) {
    varnames <- attr(x, "varnames")
    varnames <- if (is.null(varnames) || varnames %in% c("", NA) ||
                 (length(varnames) != 1)) "-" else as.character(varnames)
    return(varnames)
  } else stop("'x' must be a data frame or an atomic type")
}

#' @rdname varnames
#' @export
`varnames<-` <- function(x, value) {
  if (!is.atomic(value)) stop("'value' must be of atomic type")
  if (is.data.frame(x)) {
    len <- ncol(x)
    if (len != length(value)) {
      stop("length of 'value' not equal to number of columns in 'x'")
    }
    value <- lapply(value, function(x) if (
      is.null(x) || x %in% c("", NA) || (length(x) != 1))
      "-" else as.character(x))
    value <- unlist(value, use.names = FALSE)
    for (i in seq_len(len)) {
      attr(x[, i], "varnames") <- value[i]
    }
    return(x)
  } else if (is.atomic(x)) {
    if (length(value) != 1) {
      stop("length of 'value' must be 1 for atomic type 'x'")
    }
    value <- if (is.null(value) || value %in% c("", NA)) "-" else
      as.character(value)
    attr(x, "varnames") <- value
    return(x)
  } else stop("'x' must be a data frame or an atomic type")
}

#' @rdname varnames
#' @export
units <- function(x, names = FALSE) {
  if (is.data.frame(x)) {
    units <- lapply(x, attr, "units")
    units <- lapply(units, function(x) if (
      is.null(x) || x %in% c("", NA) || (length(x) != 1))
      "-" else as.character(x))
    units <- unlist(units, use.names = names)
    return(units)
  } else if (is.atomic(x)) {
    units <- attr(x, "units")
    units <- if (is.null(units) || units %in% c("", NA) ||
                 (length(units) != 1)) "-" else as.character(units)
    return(units)
  } else stop("'x' must be a data frame or an atomic type")
}

#' @rdname varnames
#'
#' @importFrom methods is
#' @export
`units<-` <- function(x, value) {
  if (!is.atomic(value)) stop("'value' must be of atomic type")
  if (is(x, "difftime")) stop(
    "'x' has class difftime - call base::units() instead")
  if (is.data.frame(x)) {
    len <- ncol(x)
    if (len != length(value)) {
      stop("length of 'value' not equal to number of columns in 'x'")
    }
    value <- lapply(value, function(x) if (
      is.null(x) || x %in% c("", NA) || (length(x) != 1))
      "-" else as.character(x))
    value <- unlist(value, use.names = FALSE)
    for (i in seq_len(len)) {
      attr(x[, i], "units") <- value[i]
    }
    return(x)
  } else if (is.atomic(x)) {
    if (length(value) != 1) {
      stop("length of 'value' must be 1 for atomic type 'x'")
    }
    value <- if (is.null(value) || value %in% c("", NA)) "-" else
      as.character(value)
    attr(x, "units") <- value
    return(x)
  } else stop("'x' must be a data frame or an atomic type")
}

#' Extract Parts of an Object with Varnames and Units Attributes
#'
#' Conserves `varnames` and `units` attributes of vectors and data
#' frames during extraction.
#'
#' Extraction from atomic types is done as `x[i]` ignoring `j` and
#' `drop` (applies also to matrices and arrays). Extraction from data
#' frames is done as `x[i, j, drop]`.
#'
#' @return A vector or data frame with `varnames` and `units`
#'   attributes.
#'
#' @param x An [atomic] type or a data frame. Object from which to extract
#'   element(s).
#' @param i,j Indices specifying elements to extract as specified in
#'   [Extract()].
#' @param drop A logical value. If `TRUE` (default), the result is coerced
#'   to the lowest possible dimension.
#'
#' @seealso [Extract()], [drop()] and
#'   [varnames()].
#'
#' @examples
#' xx <- data.frame(lengths = 1:3, areas = 4:6)
#' varnames(xx) <- c("lengths", "areas")
#' units(xx) <- c("m", "m2")
#' str(xx)
#'
#' # extract specified rows and columns
#' str(ex(xx, 1:2, 1:2))
#' # extract specified rows
#' str(ex(xx, 1))
#' # extract specified columns
#' str(ex(xx, , 1))
#' # extract without dropping current class
#' ex(xx, , 1, drop = FALSE)
#' # extract elements of a vector
#' ex(xx$lengths, 2:3)
#'
#' @export
ex <- function(x, i, j, drop = TRUE) {
  v <- varnames(x, names = TRUE)
  u <- units(x, names = TRUE)

  if (is.atomic(x)) {
    out <- x[i]
  } else if (is.data.frame(x)) {
    out <- x[i, j, drop = drop]
    # extracting single row with drop = TRUE results in a list
    # assignment of varnames and units to list is not defined
    if (is.list(out)) out <- as.data.frame(out)
    v <- v[j]
    u <- u[j]
  } else stop("'x' must be either atomic type or data frame")
  varnames(out) <- v
  units(out) <- u
  return(out)
}

#' Data Input with Units
#'
#' Reads tabular data from a file and represents them as data frame. Attributes
#' `varnames` (representing variable names) and `units` (representing
#' units of measurement or space efficient metadata) are assigned to each
#' column.
#'
#' `read_eddy` extends the possibilities of [read.table()] so it
#' can also read units of measurement. However, it uses default arguments of
#' [read.csv()] to accomodate loading of data for the most common
#' input type. `read_eddy` also sets useful defaults common for eddy
#' covariance (*eddy*) data. Missing values are often reported as
#' `"-9999.0"` or `"-9999"` by post-processing software, therefore
#' `na.strings = c("NA", "-9999.0", "-9999")` is used as default.
#'
#' Attribute `varnames` contains original variable name of respective
#' column without automated conversion that is done for column name. The main
#' purpose of `varnames` attribute is to provide control over conversion of
#' original column names and keep variable name of a vector when it is separated
#' from the original data frame.
#'
#' Units are expected to be one line below the header in the input file. Instead
#' of units of measurement, it is possible to include any space efficient
#' metadata that is relevant to the respective variables. E.g. format of
#' timestamp or structure of coded variable. One line below units and further in
#' the input file is the region with data. Any missing values or blank fields
#' (converted to empty strings) in the line interpreted as units will be
#' substituted by `units_fill` string instead.
#'
#' The automated check for `"-10000"` values in the data region is provided
#' by `check_input = TRUE` (default) and produces error message if the
#' value is found. The `"-10000"` values can be introduced to the dataset
#' by rounding `"-9999"` values due to the incorrect file conversion or
#' data manipulation. Using  `check_input = FALSE` will skip the check
#' (this could improve the performance for large input files).
#'
#' @return A data frame is produced with additional attributes `varnames`
#'   and `units` assigned to each respective column.
#'
#' @param file The file name with input data to be read. It can be a file name
#'   inside the current working directory, *relative* or *absolute*
#'   path or [connection]. See [read.table()] for more
#'   detailed description. Connections to anonymous file or clipboard are not
#'   allowed. To read from clipboard use `"clipboard"` string instead of
#'   connection.
#' @param header A logical value indicating whether the names of variables are
#'   included as the first line of the input file. If `FALSE`, column names
#'   and variable names of attribute `varnames` will be automatically
#'   generated.
#' @param units A logical value indicating whether the units for respective
#'   variables are included one line above the data region in the input file. If
#'   `FALSE`, the `units` attribute of each column will be set to
#'   `units_fill` string representing missing values.
#' @param sep A character that separates the fields of input. Default separator
#'   for CSV files is `","`. See [read.table()] for other
#'   options.
#' @param quote A character string that contains the quoting characters.
#' @param dec A character that specifies decimal mark used in the input.
#' @param units_fill A character string that represents missing value of
#'   `units` attribute.
#' @param na.strings A character vector of strings representing `NA` values
#'   in the input file. Blank fields are also considered to be missing values in
#'   logical, integer, numeric and complex fields.
#' @param colClasses A character vector of classes to be assumed for the columns
#'   and recycled as necessary. See [read.table()] for more detailed
#'   description.
#' @param nrows An integer specifying the maximum number of rows to read in.
#'   Negative and other invalid values are ignored.
#' @param skip An integer. The number of lines to skip in the input file before
#'   reading data.
#' @param fill A logical value. If set to `TRUE` (default), the rows that
#'   have unequal length will be corrected with blank fields.
#' @param comment.char A character that is interpreted as comment or empty
#'   string to turn off this behaviour.
#' @param check_input A logical value that determines if values in the input
#'   will be checked for erroneous `"-10000"` value. If `TRUE`
#'   (default), any encountered `"-10000"` value in the data will trigger
#'   an error message.
#' @param ... Further arguments to be passed to the internal `read.table`
#'   function
#' @seealso [read.table()] for information about further arguments
#'   passed to `read.table`.
#'
#'   [write_eddy()] to save data frame with `units` attributes
#'   specified for each column.
#' @examples
#' ## Storing timestamp metadata (format) and unit of height.
#' xx <- read_eddy(text =
#' "timestamp,height
#' %d.%m.%Y,m
#' 24.1.2015,1.70
#' 24.1.2016,1.72")
#' str(xx)
#' (varnames <- varnames(xx))
#' (units <- units(xx))
#'
#' ## Note that 'varnames' and 'units' attributes are dropped when you subset
#' ## rows but unchanged if you subset columns:
#' str(xx[, 1])
#' str(yy <- xx[1, ])
#' varnames(yy) <- varnames
#' units(yy) <- units
#' str(yy)
#'
#' ## Computations with columns also drop 'varnames' and 'units' attributes:
#' xx$date <- as.Date(xx$timestamp, units(xx$timestamp))
#' str(xx)
#'
#' ## Varnames store the original header without automated conversions:
#' aa <- read_eddy(text =
#' "u*,(z-d)/L,x_70%
#' m s-1,-,m
#' 1.412908015,-4.05E-02,153.7963035")
#' str(aa)
#'
#' ## header = FALSE and units = FALSE:
#' bb <- read_eddy(header = FALSE, units = FALSE, text =
#' "24.1.2015,1.70
#' 24.1.2016,1.72")
#' str(bb)
#'
#' @export
read_eddy <- function(file, header = TRUE, units = TRUE, sep = ",",
                      quote = "\"", dec = ".", units_fill = "-",
                      na.strings = c("NA", "-9999.0", "-9999"), colClasses = NA,
                      nrows = -1, skip = 0, fill = TRUE, comment.char = "",
                      check_input = TRUE, ...) {
  if (!missing(file) && inherits(file, "connection")) {
    if (summary(file)$description == "") {
      stop("connection to anonymous file not allowed")
    }
    if (summary(file)$description == "clipboard") {
      stop("connection to clipboard not allowed; use 'clipboard' string")
    }
  }
  read_header <- header
  units_fill <- as.character(units_fill)
  if (length(units_fill) != 1) stop("invalid 'units_fill' value")
  var_units <- read.table(file, header = read_header,  sep = sep,
                          quote = quote, dec = dec, na.strings = na.strings,
                          colClasses = "character", nrows = 1, skip = skip,
                          fill = fill, comment.char = comment.char, ...)
  if (header) {
    orig_varnames <-
      read.table(file, header = FALSE,  sep = sep, quote = quote, dec = dec,
                 na.strings = na.strings, colClasses = "character", nrows = 1,
                 skip = skip, fill = fill, comment.char = comment.char, ...)
  }
  if (header && units) {
    read_header <- FALSE
    skip <- skip + 2
  } else if (header | (!header && !units)) {
    var_units[] <- units_fill
  } else {
    skip <- 1 + skip
  }
  data <- read.table(file, header = read_header, sep = sep, quote = quote,
                     dec = dec, na.strings = na.strings,
                     colClasses = colClasses, nrows = nrows, skip = skip,
                     fill = fill, comment.char = comment.char, ...)
  # var_units (and orig_varnames) can have less columns than data
  # - this can happen if only fragment of header and units is present
  # - assuming file integrity also assumes left alignment of var_units and data
  if (header && units) names(data)[1:ncol(var_units)] <- names(var_units)
  if (units) var_units[var_units %in% c("", NA)] <- units_fill
  for (i in seq_len(ncol(var_units))) {
    varnames(data[, i]) <- if (header) orig_varnames[, i] else colnames(data)[i]
    units(data[, i]) <- var_units[, i]
  }
  if (check_input) {
    check <- as.vector(data == -10000)
    if (any(check[!is.na(check)])) {
      message("Missing data incorrectly replaced by -10000 in the input")
    }
  }
  return(data)
}

#' Infer Interval
#'
#' It selects the smallest number (representing a time interval in seconds) in
#' absolute terms from the numerical vector `x` with additional checks.
#'
#' Input values represent time interval in seconds and are expected to be the
#' result of `diff()` applied over ascending or descending date-time sequence
#' and stripped of its `"difftime"` class.
#'
#' Automated inference of interval is required both by `strptime_eddy()` and
#' `merge_eddy()`. Application in `merge_eddy()` is more complex as there are
#' more data frames to check and timestamp `diff()`s are more practical for
#' that. I.e. providing single vector of `diff()`s from multiple data frames is
#' valid, while concatenating "POSIXt" info from different tables to compute
#' `diff()`s is not.
#'
#' Returning the shortest time interval available seems to be the only practical
#' solution for automated inference, especially in cases when gaps are more
#' frequent than the actual time interval. If the shortest interval was not
#' selected, higher order functions (see above) would fail.
#'
#' Previously tested `median()` and modus estimations would work for large
#' samples, but they had their limitations. While median is simpler (and
#' implemented in R), it can select a time interval that is not present in the
#' data (see e.g. `median(c(1, 2, 4, 5))`). Modus was implemented (now removed)
#' to better estimate the most common time interval (preferred over median).
#' However, both modus and median fail to provide correct estimate if gaps
#' prevail in the input.
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value specifying the time interval (in seconds).
#'
#' @examples
#' openeddy:::infer_interval(c(1:5))
#' openeddy:::infer_interval(-c(1:5))
#' try(openeddy:::infer_interval(0))
#' try(openeddy:::infer_interval(c(-1,1)))
#' openeddy:::infer_interval(c(2, 1, 1, 2))
#' openeddy:::infer_interval(NULL)
#' openeddy:::infer_interval(NA)
#'
#' @keywords internal
#' @noRd
infer_interval <- function(x) {
  if (length(x) == 0) return(NULL)
  # choose the smallest interval in absolute terms (descending series possible)
  # works for `x` with or without gaps
  # higher level functions should check:
  # - for presence of gaps
  # - for validity of obtained interval (and correctly report issues)
  #   - other than this one will throw error for sure
  #   - even this interval might be invalid
  res <- x[which.min(abs(x))] # it does not distinguish ascending/descending
  # NA would make issues in the following checks
  # - this case should not occur due to checks in higher level functions
  if (length(res) == 0) return(NA_real_)
  # interval of timestamp should be non-zero (ascending or descending)
  if (res == 0) {
    stop("date-time sequence includes duplicated timestamp")
  }
  # interval can be positive or negative (not both)
  # this condition might be relaxed (removed) as it needs to be checked anyway in strptime_eddy() for manual intervals and unordered timestamp can be valid in merge_eddy() ----
  # what about merge_eddy() with duplicated records?
  # - likely no issue as duplications would still have valid diffs in each data frame
  if (any(x < 0) & any(x > 0)) {
    stop("date-time sequence is both ascending and descending")
  }
  return(res)
}

#' Conversion of Regular Date-time Sequence from Character
#'
#' Converts character vector to class `"POSIXct"` using
#' [strptime()] and validates the result. The input has to represent an
#' ascending or descending regular date-time sequence with given time interval.
#'
#' Eddy covariance related measurements are usually stored with a timestamp
#' representing the end of the averaging period (typically 1800 s) in standard
#' time. This can however cause difficulties during data aggregation or
#' plotting. Therefore it is recommended to shift the date-time information
#' using `shift.by` to represent the center of averaging period prior to
#' any computations. It is also recommended to change the date-time information
#' to its original state before saving to a file (see Examples section).
#'
#' Any unsuccessful attempt to convert date-time information is considered to be
#' unexpected behavior and returns an error message instead of `NA` value.
#' In case that multiple formats are present in the timestamp, it has to be
#' corrected prior using `strptime_eddy()`. It is expected that time series
#' are continuous even if no valid measurements are available for given time
#' interval. Therefore `interval` value is checked against the lagged
#' differences ([diff()]) applied to the converted date-time vector
#' and returns an error message if mismatch is found. If `allow_gaps =
#' TRUE`, date-time information does not have to be regular but time differences
#' must be multiples of `interval`.
#'
#' If `interval = NULL`, automated recognition of `interval` is applied. This is
#' preferred to setting `interval` value manually. Only in rare cases when
#' `allow_gaps = TRUE` and original time interval is not present in `x`, it is
#' not possible to infer the original time interval. Even in that case,
#' `strptime_eddy()` will execute successfully. The inferred interval represents
#' the shortest time interval present among `x` records.
#'
#' The default [`storage.mode`] of returned `"POSIXct"` vector is set to be
#' `"integer"` instead of `"double"`. This simplifies the application of
#' [`round_df()`] (it avoids rounding) but could lead to an unexpected behavior
#' if the date-time information is expected to resolve fractional seconds (it
#' [`trunc()`]ates decimals).
#'
#' @return A `"POSIXct"` vector with assigned attributes `varnames` and `units`
#'   specified as `"timestamp"` and `"-"`, respectively.
#'
#' @param x A character vector containing date-time information to be converted
#'   to class `"POSIXct"`.
#' @param format A character string. The default `format` is
#'   `"%Y-%m-%d %H:%M"`
#' @param interval A numeric value specifying the time interval (in seconds)
#'   valid for all values of the input date-time vector. If `NULL` (default),
#'   `interval` is inferred from the data (see Details).
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If `TRUE`, date-time information does
#'   not have to be regular but time differences must be multiples of
#'   `interval`.
#' @param tz A time zone (see [time zones]) specification to be used
#'   for the conversion.
#' @param storage.mode A character string. Either `"integer"` (default) or
#'   `"double"` (see Details).
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @seealso [strptime()] provides the details about conversions
#'   between date-time character representation and `"POSIXct"` or
#'   `"POSIXlt"` classes. It also includes information about `format`
#'   *conversion specification*.
#'
#'   [DateTimeClasses] further inform about the date-time classes.
#'
#'   See [locales] to query or set a locale.
#' @examples
#' xx <- c("01.01.2014  00:30:00", "01.01.2014  01:00:00",
#' "01.01.2014  01:30:00", "01.01.2014  02:00:00")
#' str(xx)
#' (yy <- strptime_eddy(xx, "%d.%m.%Y %H:%M", shift.by = -900L))
#' attributes(yy)
#' typeof(yy)
#'
#' ## Convert to original format
#' format(yy + 900, format = "%d.%m.%Y %H:%M:%S", tz = "GMT")
#'
#' ## Handle gaps in timestamp
#' zz <- xx[-3]
#' strptime_eddy(zz, "%d.%m.%Y %H:%M", allow_gaps = TRUE)
#'
#' ## This is not a regular date-time sequence
#' try(strptime_eddy(zz, "%d.%m.%Y %H:%M")) # error returned
#'
#' ## Interval argument provided incorrectly
#' try(strptime_eddy(xx, "%d.%m.%Y %H:%M", interval = 3600)) # error returned
#'
#' @export
strptime_eddy <- function(x, format = "%Y-%m-%d %H:%M", interval = NULL,
                          shift.by = NULL, allow_gaps = FALSE, tz = "GMT",
                          storage.mode = "integer", ...) {
  if (anyNA(x)) stop("NAs in 'x' not allowed")
  if (!is.null(interval) && (interval == 0 | is.na(interval))) {
    stop("wrong value of 'interval'")
  }
  out <- as.POSIXct(strptime(x, format = format, tz = tz, ...))
  # anyNA(NULL) is false
  if (anyNA(out)) {
    # provide info where first issue arose
    i <- which(is.na(out))[1]
    stop("incorrect 'format' or multiple formats present in 'x'\n",
         "  - first issue for x[", i, "]: ", x[i], " (expected ", format, ")")
  }
  tdiff <- diff(as.numeric(out)) # strip POSIXt class to get numeric diff()s
  if (any(tdiff < 0) & any(tdiff > 0)) {
    stop("date-time sequence is both ascending and descending")
  }
  if (is.null(interval)) {
    # the only need for manual interval seems explicit validity check for given
    # allow_gaps setting
    interval <- infer_interval(tdiff)
  }
  # if x has length 0 or 1 (no interval), skip further testing conditions
  # - interval can be set by user so test on tdiff
  # - 0 length of output is also covered by il0
  il0 <- ifelse(length(tdiff) == 0, TRUE, FALSE)
  # first test that timestamp forms regular sequence with 'interval' multiples
  # - needed both for manually set interval and inferred one
  # - gaps should be allowed only if they are multiples of interval
  if (!il0 && any(tdiff %% interval != 0)) {
    stop("intervals among timestamps are not multiples of set 'interval' ",
         "(interval = ", interval, ")\n",
         "  - intervals present in 'x': ",
         paste0(unique(tdiff), collapse = ", "))
  }
  # catch cases when timestamp gaps are present but allow_gaps = FALSE
  # - gaps can be explicit (multiple tdiff values) or implicit (lower manually
  #   set interval than actual one)
  if (!il0 && !allow_gaps &&
      (length(unique(tdiff)) > 1 | interval != infer_interval(tdiff))) {
    i <- which(tdiff != interval)[1]
    stop("timestamp in 'x' contains gaps (interval = ", interval, ")\n",
         "  - first gap for x[", i, ":", i + 1, "]: ", x[i], " - ", x[(i + 1)])
  }
  # catch cases when timestamp gaps are allowed and present
  # - timestamp without gaps should have only one unique tdiff value
  if (!il0 && allow_gaps && length(unique(tdiff)) > 1) {
    message("timestamp in 'x' contains gaps (interval = ", interval, ")")
  }
  if (!is.null(shift.by)) out <- out + shift.by
  # Set storage mode of timestamp to integer to simplify data frame rounding
  storage.mode(out) <- storage.mode
  varnames(out) <- "timestamp"
  units(out) <- "-"
  return(out)
}

#' Eddy Covariance Data Output
#'
#' Facilitates printing object `x` also with its units of measurement (or
#' space efficient metadata) to a file or [connection].
#'
#' `write_eddy` extends the possibilities of `write.table` so the
#' units of measurement can also be written. However, it uses default arguments
#' of `write.csv` to provide flexibility for the user and to accomodate the
#' function for the most common case. The character string `"-9999"` is
#' typically used to represent missing values in eddy covariance (*eddy*)
#' data.
#'
#' Storing `varnames` and `units` attributes is practical mostly
#' within data frames and vectors. Attribute `varnames` extracted from each
#' data frame column represents names of respective variables and its main
#' purpose is to keep variable names of isolated vectors. Attribute `units`
#' extracted from each column represents units of measurement (or space
#' efficient metadata) of respective variables that are written one line above
#' data region. If the `varnames` or `units` attribute of given column
#' is `NULL`, of length not equal to 1, or contains missing value or empty
#' string, it is not considered meaningful. In that case the default column name
#' produced by [as.data.frame()] is used instead (considered only if
#' `x` is supplied as vector) and unit of measurement is substituted with
#' `units_fill` string. `units_fill` can be an empty string.
#'
#' Units of measurement are considered to be part of the output header and
#' therefore `col.names` and `quote` arguments have the effect on the
#' way they are written.
#'
#' @param x The object that will be written. It is a data frame with optional
#'   attributes `units` and `varnames` assigned to each column.
#'   Otherwise it is converted by [as.data.frame()].
#' @param file Either a character string naming a file to write to or a
#'   [connection] that is open for writing. `""` results in
#'   writing to the console.
#' @param append A logical value. It is considered only if `file` is not a
#'   `connection`. If `TRUE`, the output is written below the content
#'   of the file. If `FALSE`, the content of the file is overwritten.
#' @param quote A logical value (`TRUE` or `FALSE`) or a numeric
#'   vector. If `TRUE`, columns of class character or factor will be
#'   surrounded by double quotes. If a numeric vector, its elements should mark
#'   the indices of character or factor columns to quote. In both cases, row and
#'   column names and units are quoted if present. If `FALSE`, no quoting
#'   is performed.
#' @param sep A character used as the field separator of each row.
#' @param units_fill A character string that represents missing value of
#'   `units` attribute in the output.
#' @param na A character string that represents missing data values in the
#'   output.
#' @param row.names Either a logical value (`TRUE` or `FALSE`) that
#'   determines if the row names of `x` should be included in the output,
#'   or a character vector of row names that will be used instead.
#' @param col.names Either a logical value (`TRUE`, `FALSE` or
#'   `NA`) or a character vector. If `TRUE`, column names of `x`
#'   will be included in the output. If a character vector, its elements will be
#'   used as column names. If `x` is supplied as vector, an attempt is made
#'   to extract meaningful variable name from its attribute `varnames`. In
#'   all cases, units extracted from `units` attribute of each column will
#'   be written one line below column names with identical format. See the 'CSV
#'   files' section in [write.table()] for further explanation of
#'   `col.names = NA`.
#' @param qmethod A character string. It determines the way how strings quoting
#'   is performed in case of embedded double quote characters. The options are
#'   either `"double"` (`write.csv` and `write.csv2` defaults),
#'   that doubles the quote character, or `"escape"` (`write.table`
#'   default), that escapes it in C style by a backslash.
#' @param ... Further arguments to be passed to the internal
#'   [write.table()] function.
#' @seealso [write.table()] for information about full list of allowed
#'   arguments and their descriptions.
#'
#'   [read_eddy()] to read data frame with `varnames` and
#'   `units` attributes specified for each column.
#' @examples
#' xx <- read_eddy(text =
#' "timestamp,height,weight
#' %d.%m.%Y,m,kg
#' 24.1.2015,1.70,75
#' 24.1.2016,1.72,78")
#' str(xx)
#' write_eddy(xx, "")
#'
#' ## NB: 'varnames' and 'units' attributes are dropped when you subset rows
#' ## but unchanged if you subset columns:
#' write_eddy(xx[, 1], "") # 'varnames' attribute of the vector used as column name
#' write_eddy(head(xx), "") # dropped 'units' attribute
#'
#' # Example of using "col.names = NA"
#' f <- file.path(tempdir(), "ex.csv")
#' zz <- file(f, "w")  # open an output file connection
#' write_eddy(xx, zz, row.names = TRUE, col.names = NA)
#' close(zz)
#' (ex_data <- read_eddy(f, row.names = 1))
#' str(ex_data)
#' unlink(f)
#'
#' @importFrom utils write.table
#' @export
write_eddy <- function(x, file = "", append = FALSE, quote = TRUE, sep = ",",
                       units_fill = "-", na = "-9999", row.names = FALSE,
                       col.names = TRUE, qmethod = "double", ...) {
  units_fill <- as.character(units_fill)
  if (length(units_fill) != 1) stop("invalid 'units_fill' value")
  if (isTRUE(col.names) || is.na(col.names) || is.character(col.names)) {
    if (!is.data.frame(x)) {
      x <- data.frame(x)
      if (length(x) == 1) {
        varname <- attr(x[, 1], "varnames")
        varname <- if (is.null(varname) || varname %in% c("", NA) ||
                       (length(varname) != 1)) FALSE else as.character(varname)
        names(x)[varname != FALSE] <- varname[varname != FALSE]
      }
    }
    units <- lapply(x, attr, "units")
    units <- lapply(units, function(x) if (
      is.null(x) || x %in% c("", NA) || (length(x) != 1))
      units_fill else as.character(x))
    header <- as.data.frame(units, stringsAsFactors = FALSE, optional = TRUE)
    q_header <- if (isTRUE(quote) || is.numeric(quote)) TRUE else quote
    rn <- FALSE
    rnames <- NULL
    if (is.logical(row.names)) {
      if (row.names) rn <- TRUE
    } else {
      rnames <- as.character(row.names)
      rn <- TRUE
      if (length(rnames) != nrow(x))
        stop("invalid 'row.names' specification")
    }
    if (is.logical(col.names)) {
      if (!rn && is.na(col.names))
        stop("'col.names = NA' makes no sense when 'row.names = FALSE'")
    }
    rn_header <- if (is.na(col.names) && rn) c("") else FALSE
    if (append) warning("appending column names to file")
    write.table(header, file, append = append, quote = q_header, sep = sep,
                na = na, row.names = rn_header, col.names = col.names,
                qmethod = qmethod, ...)
    append <- TRUE
  }
  write.table(x, file, append = append, quote = quote, sep = sep, na = na,
              row.names = row.names, col.names = FALSE,
              qmethod = qmethod, ...)
}

#' Correct Character Vector
#'
#' Substitute given characters or strings by their alternatives.
#'
#' The function is intended to simplify the use of variable names and units
#' within the typical processing workflow employing
#' [openeddy::openeddy].
#'
#' If `attr = "names"`, correction is meant to arrive to syntactically
#' valid names with a higher level of control. This assumes that the original
#' names were preserved during data loading (e.g. by using `check.names =
#' FALSE` in [read_eddy()] or [read.table()]). Specifically,
#' literal strings are renamed as:
#' * `"(z-d)/L"` by `"zeta"`
#' * `"qc_Tau"` by `"qc_Tau_SSITC"`
#' * `"qc_H"` by `"qc_H_SSITC"`
#' * `"qc_LE"` by `"qc_LE_SSITC"`
#' * `"qc_co2_flux"` by `"qc_NEE_SSITC"`
#'
#' and specified patterns or characters withing strings are substituted using
#' regular expression patterns:
#' * `"co2_flux"` by `"NEE"`
#' * `"*"` by `"star"`
#' * `"%"` by `"perc"`
#' * `"-"` and `"/"` by `"_"`.
#'
#' After the substitutions `make.names(names = x, ...)` is executed.
#'
#' If `attr = "units"`, round and square brackets are substituted by an
#' empty string.
#'
#' @param x A character vector.
#' @param attr A character string identifying an attribute type a character
#'   vector `x` to correct. Can be abbreviated.
#' @param ... Further arguments to be passed to the internal
#'   [make.names()] function.
#'
#' @return A corrected character vector.
#'
#' @seealso [make.names()].
#'
#' @examples
#' correct(c("co2_flux", "qc_co2_flux", "(z-d)/L", "x_70%", "*[-(z-d)/L"))
#' correct(c("qc_co2_flux", "qc_NEE_SSITC"), unique = TRUE)
#' correct(c("[m]", "(s)", "kg"), attr = "units")
#'
#' @export
correct <- function(x, attr = c("names", "units"), ...) {
  attr <- match.arg(attr)
  if (attr == "names") {
    x[x == "(z-d)/L"] <- "zeta"
    x[x == "qc_Tau"] <- "qc_Tau_SSITC"
    x[x == "qc_H"] <- "qc_H_SSITC"
    x[x == "qc_LE"] <- "qc_LE_SSITC"
    x[x == "qc_co2_flux"] <- "qc_NEE_SSITC"
    x <- gsub("co2_flux", "NEE", x) # assumption: co2_flux = NEE
    x <- gsub("\\*", "star", x) # ustar, Tstar
    x <- gsub("\\%", "perc", x) # signal contribution percentages
    x <- gsub("\\-|\\/", "_", x)
    x <- make.names(names = x, ...)
  }
  if (attr == "units") {
    x <- gsub(c("\\[|\\]|\\(|\\)"), "", x) # remove brackets
  }
  return(x)
}

#' Combine Quality Checking Results
#'
#' Combine quality checking results depending on whether they have a fixed or
#' cumulative effect or any combination of these effects. It is also checked how
#' should `NA`s be interpreted.
#'
#' The quality checking results to combine must be provided as columns of a data
#' frame `x`, optionally with any number of further columns that will be
#' ignored. Columns specified by `qc_names` will be further separated
#' according to their additivity. For flags with fixed effect (`additive =
#' FALSE`; the most typical type), maximum is taken over each row. For flags
#' with additive effect (`additive = TRUE`), sum is taken over each row. In
#' case both types of flags are present, results for both groups are summed
#' together.
#'
#' The most typical value of argument `na.as` is `NA`. `NA` value
#' does not suggest any change in interpretation (value of variable
#' corresponding to this flag will be removed within quality checking scheme).
#' Exceptionally, value `0` can be used in case that the `NA` flag of
#' the quality checking test/filter is an expected result and means that the
#' half-hour was not checked by the given test/filter (e.g.
#' [despikeLF()]).
#'
#' @section Automated recognition: Default values for `additive` and
#'   `na.as` arguments are `FALSE` and `NA`, respectively. In
#'   case that `additive_pattern` is found within `qc_names` (i.e.
#'   `qc_names` ending with `"interdep"` or `"wresid"` pattern),
#'   respective values of `additive` are changed to `TRUE`. This is
#'   because [interdep()] and wresid (see [extract_QC()])
#'   quality control checks are defined as additive within the current quality
#'   control scheme. If `na.as_0_pattern` is detected within
#'   `qc_names` (i.e. `qc_names` ending with `"spikesLF"`,
#'   `"fetch70"` or `"man"` pattern), respective values of
#'   `na.as` are changed to `0` (see [despikeLF()]).
#'
#' @return An integer vector with attributes `varnames` and `units` is
#'   produced. `varnames` value is set by `name_out` argument. Default
#'   value of `varnames` and `units` is set to `"-"`.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame `x` columns to combine.
#' @param name_out A character string providing `varnames` value of the
#'   output.
#' @param additive `NULL` or a vector of logical values (`TRUE` or
#'   `FALSE`) determining additivity of each respective column of `x`
#'   given by `qc_names`. If `NULL`, automated recognition is used.
#'   Otherwise, values determine if the flags should be treated as additive
#'   (`additive = TRUE`) or with fixed effect (`additive = FALSE`). If
#'   only one value is provided, all columns are considered to be of the same
#'   type.
#' @param na.as `NULL` or a vector of integer or `NA` values
#'   determining interpretation of missing flags in each respective column of
#'   `x` given by `qc_names`. If `NULL`, automated recognition is
#'   used. If only one value is provided, all columns are treated the same way.
#' @param additive_pattern A character string. A [regular
#'   expression][regexp] [grep()] `pattern` identifying `qc_names`
#'   of flags with additive effect.
#' @param na.as_0_pattern A character string. A [regular
#'   expression][regexp] [grep()] `pattern` identifying `qc_names`
#'   for which `NA` flags are interpreted as zeros.
#' @param no_messages A logical value.
#'
#' @seealso [summary_QC()].
#'
#' @examples
#' set.seed(5)
#' aa <- data.frame(xx = sample(c(0:2, NA), 20, replace = TRUE))
#' aa$yy <- sample(c(0:2, NA), 20, replace = TRUE)
#' aa$add_F <- combn_QC(aa, qc_names = c("xx", "yy"), additive = FALSE,
#' name_out = "add_F")
#' aa$add_T <- combn_QC(aa, qc_names = c("xx", "yy"), additive = TRUE,
#' name_out = "add_T")
#' aa$add_F_na.as_0 <- combn_QC(aa, qc_names = c("xx", "yy"), additive = FALSE,
#' na.as = 0, name_out = "add_F_na.as_0")
#' aa$add_F_na.as_0part <- combn_QC(aa, qc_names = c("xx", "yy"),
#' additive = FALSE, na.as = c(0, NA), name_out = "add_F_na.as_0part")
#' aa$add_F_na.as_2 <- combn_QC(aa, qc_names = c("xx", "yy"), additive = FALSE,
#' na.as = 2, name_out = "add_F_na.as_2")
#' str(aa)
#' aa
#'
#' @export
combn_QC <- function(x, qc_names, name_out = "-", additive = NULL,
                     na.as = NULL, additive_pattern = "interdep$|wresid$",
                     na.as_0_pattern = "spikesLF$|fetch70$|man$",
                     no_messages = FALSE) {
  x_names <- colnames(x)
  name_out <- as.character(name_out[1])
  qc_names <- as.character(qc_names)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!all(qc_names %in% x_names)) {
    stop(paste("missing", paste0(qc_names[!(qc_names %in% x_names)],
                                 collapse = ", ")))
  }
  if (is.null(additive)) {
    additive <- grepl(additive_pattern, qc_names)
    if (!no_messages) {
      if (sum(additive)) {
        message("detected columns with additive effect: ",
                paste0(qc_names[additive], collapse = ", "))
      } else message("no columns with additive effect detected")
    }
  } else {
    if (!is.logical(additive) || anyNA(additive) || length(additive) == 0) {
      stop("'additive' must be logical vector with non-missing values")
    }
  }
  if (is.null(na.as)) {
    na.as <- rep(NA, length(qc_names))
    na.as_0 <- grep(na.as_0_pattern, qc_names)
    na.as[na.as_0] <- 0L
    if (!no_messages) {
      if (length(na.as_0)) {
        message("detected columns with 'na.as = 0': ",
                paste0(qc_names[na.as_0], collapse = ", "))
      } else message("no columns with 'na.as = 0' detected")
    }
  } else {
    if (length(na.as) == 0 || (!is.numeric(na.as) && !all(is.na(na.as)))) {
      stop("'na.as' must be a vector containing numeric or NA values")
    }
  }
  if (length(additive) > 1) {
    if (length(qc_names) != length(additive)) {
      stop("'additive' must be of same lenght as 'qc_names' or length 1")
    }
  }
  if (length(na.as) > 1) {
    if (length(qc_names) != length(na.as)) {
      stop("'na.as' must be of same lenght as 'qc_names' or length 1")
    }
  } else if (length(na.as) != length(qc_names)) {
    na.as <- rep(na.as, length(qc_names))
  }
  df <- x[c(qc_names)]
  if (length(qc_names) == 0) {
    return(df)
  }
  if (any(df > 2, na.rm = TRUE) || any(df < 0, na.rm = TRUE))
    stop("QC flags must be within range 0 - 2")
  if (!all(is.na(na.as))) {
    for (i in seq_along(qc_names)) {
      df[is.na(df[i]), i] <- na.as[i]
    }
  }
  out <- df[, FALSE]
  if (any(additive)) {
    add <- df[additive]
    add_allNA <- allNA(add, 1)
    out$add[!add_allNA] <- rowSums(add[!add_allNA, , drop = FALSE])
  }
  if (any(!additive)) {
    abs <- df[!additive]
    abs_allNA <- allNA(abs, 1)
    out$abs[!abs_allNA] <- apply(abs[!abs_allNA, , drop = FALSE], 1, max)
  }
  out_allNA <- allNA(out, 1)
  out[!out_allNA, name_out] <- rowSums(out[!out_allNA, , drop = FALSE])
  out[, name_out][out[name_out] > 2] <- 2L
  out[, name_out] <- as.integer(out[, name_out])
  attr(out[, name_out], "varnames") <- name_out
  attr(out[, name_out], "units") <- "-"
  return(out[, name_out])
}

#' Apply Storage Flux Correction
#'
#' Correction of matter or energy flux (`flux`) with storage computed using
#' discrete (one point) approach (`st`) or profile measurement of CO2
#' concentration (`stp`).
#'
#' If both storage estimates are available, `stp` takes priority. If both
#' `st` and `stp` estimates are `NA`, original flux value is
#' kept. `flux`, `st` and `stp` (if not NULL) must have the same
#' length.
#'
#' @return A vector with attributes `varnames` and `units` is
#'   produced. `varnames` value is set by `name_out` argument.
#'   `units` value is extracted from `flux` vector by
#'   [units()] or set to default `"-"`.
#'
#' @param flux A numeric vector with flux values.
#' @param name_out A character string providing `varnames` value of the
#'   output.
#' @param st A numeric vector with storage computed using discrete
#'   (one point) approach.
#' @param stp A numeric vector with storage computed using
#' profile measurement of CO2.
#'
#' @seealso [units()].
#'
#' @examples
#' aa <- matrix(ncol = 3, nrow = 10, byrow = TRUE, c(-1, 1, 2),
#'              dimnames = list(NULL, c("flux", "st", "stp")))
#' aa[c(4, 8, 9, 11, 15, 18, 22, 25, 27, 29)] <- NA
#' (aa <- as.data.frame(aa))
#' aa$flux_stc <- add_st(aa$flux, aa$st, aa$stp, "flux_stc")
#' aa
#' lapply(aa, attributes)
#'
#' @export
add_st <- function(flux, st, stp = NULL, name_out = "-") {
  if (length(flux) != length(st)) {
    stop("'flux' and 'st' must be of the same length")
  }
  if (!is.numeric(flux) || !is.numeric(st)) {
    stop("'flux' and 'st' must be numeric vectors")
  }
  if (!is.null(stp)) {
    if (length(flux) != length(stp)) {
      stop("'stp' must be of the same length as 'flux'")
    }
    if (!is.numeric(stp)) stop("'stp' must be a numeric vector")
  }
  units <- units(flux)
  out <- flux
  out[!is.na(st)] <- flux[!is.na(st)] + st[!is.na(st)]
  if (!is.null(stp)) {
    out[!is.na(stp)] <- flux[!is.na(stp)] + stp[!is.na(stp)]
  }
  varnames(out) <- name_out
  units(out) <- units
  return(out)
}

#' Create Input for REddyProc/Online Tool
#'
#' Creates input for gap-filling and flux partitioning tools implemented either
#' offline in R (`REddyProc` package) or accessible online ([Online
#' Tool](https://bgc.iwww.mpg.de/5622399/REddyProc)) from the data frame `x`.
#'
#' The data frame `x` is expected to have certain properties. It is required
#' that it has column names and contains column named `"timestamp"` of class
#' `"POSIXt"` with regular sequence of date-time values with (half-)hourly time
#' interval. Any missing values in `"timestamp"` are not allowed. Thus, if no
#' records exist for given date-time value, it still has to be included. It also
#' has to contain column names specified by `names_in` (respective to
#' `names_out`). Default vector of `names_out` represents a typical set of
#' variables used in the processing tools but can be modified. Minimum
#' requirement is for the data frame `x` to include timestamp and global
#' radiation. Columns of data frame `x` ideally have assigned attributes
#' `varnames` and `units`.
#'
#' The typical variables (column names; i.e. `names_out`) expected by the
#' processing tools (name; unit) are quality control of net ecosystem exchange
#' (`"qcNEE"`; `"-"`), net ecosystem exchange (`"NEE"`; `"umol m-2 s-1"`),
#' quality control of latent heat (`"qcLE"`; `"-"`), latent heat (`"LE"`; `"W
#' m-2"`), quality control of sensible heat (`"qcH"`; `"-"`), sensible heat
#' (`"H"`; `"W m-2"`), global radiation (`"Rg"`; `"W m-2"`), air temperature
#' (`"Tair"`; `"degC"`), soil temperature (`"Tsoil"`; `"degC"`), relative
#' humidity (`"rH"`; `"%"`), vapor pressure deficit(`"VPD"`; `"hPa"`), quality
#' control of momentum flux (`"qcTau"`; `"-"`) and friction velocity (`"Ustar"`;
#' `"m s-1"`). The unicode character for a greek letter micro (e.g. in NEE
#' units) is not accepted by the processing tools, thus it is substituted by
#' simple `"u"`. Check the processing tools
#' [documentation](https://bgc.iwww.mpg.de/5622399/REddyProc) for more details.
#'
#' `time_format` has two available options. `"YDH"` (default) extracts columns
#' Year, DoY (Day of year) and Hour (decimal number) from the timestamp of `x`.
#' It is less informative than `"YMDHM"` format but it is supported by all
#' versions of both offline and online tools. `"YMDHM"` extracts columns Year,
#' Month, Day, Hour, Minute. This format is not accepted by the current [Online
#' Tool](https://bgc.iwww.mpg.de/5624918/Input-Format).
#'
#' Fluxes are always filtered with respective quality control flags if provided.
#' In case of `"qcTau"`, quality control is applied to friction velocity
#' (`"Ustar"`). In case of `"NEE"`, it is filtered according to `"qcNEE"` flags
#' and if `qcTau_filter = TRUE` also according to `"qcTau"` flags. This
#' conservative approach will assure that NEE values that cannot be compared
#' against friction velocity threshold (Ustar filtering) will be excluded.
#'
#' @param x A data frame with column names and `"timestamp"` column in POSIXt
#'   format.
#' @param names_in A character vector. Column names (variables) present in `x`
#'   that will be used as input.
#' @param names_out A character vector. Column names required by the tools for
#'   respective `names_in`.
#' @param time_format A character string identifying supported time format of
#'   the output. Can be abbreviated.
#' @param hourly A logical value indicating temporal resolution of timestamp. If
#'   `FALSE` (default), half-hourly resolution is expected.
#' @param qcTau_filter A logical value indicating whether NEE should be filtered
#'   using qcTau flags. See details.
#'
#' @seealso [read_eddy()] and [write_eddy()].
#'
#' @encoding UTF-8
#' @export
set_OT_input <- function(x,
                         names_in,
                         names_out = c("qcNEE", "NEE", "qcLE", "LE", "qcH",
                                       "H", "Rg", "Tair", "Tsoil", "rH", "VPD",
                                       "qcTau", "Ustar"),
                         time_format = c("YDH", "YMDHM"),
                         hourly = FALSE,
                         qcTau_filter = TRUE) {
  x_names <- names(x)
  time_format <- match.arg(time_format)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  names_in <- as.character(names_in)
  req_vars <- c("timestamp", names_in)
  if (!all(req_vars %in% x_names)) {
    stop(paste("missing", paste0(req_vars[!(req_vars %in% x_names)],
                                 collapse = ", ")))
  }
  names_out <- as.character(names_out)
  if (length(names_in) != length(names_out)) {
    stop("length(names_in) and length(names_out) have to be equal")
  }
  if (!inherits(x$timestamp, "POSIXt")) {
    stop("'x$timestamp' must be of class 'POSIXt'")
  }
  if (anyNA(x$timestamp)) stop("NAs in 'x$timestamp' not allowed")
  tdiff <- unique(diff(as.integer(x$timestamp)))
  if (length(tdiff) > 1) {
    stop("'x$timestamp' does not form regular sequence")
  }
  interval <- ifelse(hourly, 3600L, 1800L)
  # make it work also for one row of data (tdiff of 0 length)
  if (length(tdiff) && tdiff != interval) {
    stop("timestamp expected in ", ifelse(hourly, "hourly", "half-hourly"),
         " interval")
  }
  ts <- as.POSIXlt(x$timestamp)
  x <- x[names_in]
  units <- gsub("\u00B5", "u", units(x))
  for (i in seq_len(ncol(x))) {
    varnames(x[, i]) <- names_in[i]
    units(x[, i]) <- units[i]
  }
  names(x) <- names_out
  if (hourly) {
    if (!all(ts$min %in% 0)) {
      stop("Timestamp minutes are not in required format 'XX:00'")
    }
  } else {
    if (!all(ts$min %in% c(0, 30))) {
      stop("Timestamp minutes are not in required format 'XX:00' or 'XX:30'")
    }
  }
  if (!("Rg" %in% names_out)) stop("Global radiation not provided")
  out <- if (time_format == "YDH") {
    data.frame(Year = ts$year + 1900L, DoY = ts$yday + 1L,
               Hour = ts$hour + ts$min / 60)
  } else {
    data.frame(Year = ts$year + 1900L, Month = ts$mon + 1L, Day = ts$mday,
               Hour = ts$hour, Minute = ts$min)
  }
  for (i in seq_len(ncol(out))) {
    varnames(out[, i]) <- names(out)[i]
    units(out[, i]) <- "-"
  }
  out <- cbind(out, x)
  if (anyNA(out$Rg)) {
    message("NAs in Rg - consider gap-filling global radiation")
  }
  if ("VPD" %in% names_out) {
    if (!all(is.na(out$VPD)) && any(out$VPD[!is.na(out$VPD)] > 100)) {
      message("VPD input units are probably not in hPa")
    }
  }
  if (!("Ustar" %in% names_out)) {
    message("Ustar not provided")
  } else {
    if ("qcTau" %in% names_out) {
      out$Ustar <- apply_QC(out$Ustar, out$qcTau)
    } else message("qcTau not provided: Ustar used without QC")
  }
  if (!("H" %in% names_out)) {
    message("H not provided")
  } else {
    if ("qcH" %in% names_out) {
      out$H <- apply_QC(out$H, out$qcH)
    } else message("qcH not provided: H used without QC")
  }
  if (!("LE" %in% names_out)) {
    message("LE not provided")
  } else {
    if ("qcLE" %in% names_out) {
      out$LE <- apply_QC(out$LE, out$qcLE)
    } else message("qcLE not provided: LE used without QC")
  }
  if (!("NEE" %in% names_out)) {
    message("NEE not provided")
  } else {
    if ("qcNEE" %in% names_out) {
      out$NEE <- apply_QC(out$NEE, out$qcNEE)
    } else message("qcNEE not provided: NEE used without QC")
    if (qcTau_filter && "qcTau" %in% names_out) {
      # filtering using qcTau is expected to have same effect as with
      # is.na(out$Ustar) in earlier implementation
      out$NEE <- apply_QC(out$NEE, out$qcTau)
    } else message("qcTau filtering not applied to NEE")
  }
  return(out)
}

#' Quality Control Summary
#'
#' `summary_QC` is a function that summarizes quality checking results in a
#' form of table or plot.
#'
#' `summary_QC` loads a data frame `x`, extracts quality control (QC)
#' columns from it based on `qc_names` and creates a table (`plot =
#' FALSE`) or a plot (`plot = TRUE`) for these columns. Results are
#' displayed as percentages (`perc = TRUE`) or counts (`perc = FALSE`)
#' for given flag and QC filter.
#'
#' `cumul = TRUE` specifies that cumulative effect of gradually applied QC
#' filters on resulting flags is considered. Note that for `cumul = TRUE`
#' the results do depend on the order of qc_names. `additive` is considered
#' only if `cumul = TRUE`, otherwise skipped.
#'
#' For a detailed description of automated recognition see
#' [combn_QC()].
#'
#' @return A table or a ggplot object depending on the `plot` argument
#'   value. If `length(qc_names) == 0`, `NULL` is returned instead.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame `x` columns to combine.
#' @param cumul A logical value that determines if cumulative (`cumul =
#'   TRUE`) or individual (`cumul = FALSE`) effects of quality control
#'   flags should be shown.
#' @param plot A logical value. If `TRUE`, the results are represented as a
#'   ggplot object. If `FALSE`, they are represented as a table.
#' @param perc A logical value. If `TRUE`, the results are reported in
#'   percentages. If `FALSE`, counts are used instead.
#' @param flux A character string. Used only if `plot = TRUE`. Includes the
#'   flux name in the plot title to emphasize the relevance of displayed quality
#'   control filters.
#' @param na.as `NULL` or a vector of integer or `NA` values
#'   determining interpretation of missing flags in each respective column of
#'   `x` given by `qc_names`. If `NULL`, automated recognition is
#'   used. If only one value is provided, all columns are treated the same way.
#' @param na.as_0_pattern A character string. A [regular
#'   expression][regexp] [grep()] `pattern` identifying `qc_names`
#'   for which `NA` flags are interpreted as zeros.
#' @param additive `NULL` or a vector of logical values (`TRUE` or
#'   `FALSE`) determining additivity of each respective column of `x`
#'   given by `qc_names`. If `NULL`, automated recognition is used.
#'   Otherwise, values determine if the flags should be treated as additive
#'   (`additive = TRUE`) or with fixed effect (`additive = FALSE`). If
#'   only one value is provided, all columns are considered to be of the same
#'   type.
#' @param additive_pattern A character string. A [regular
#'   expression][regexp] [grep()] `pattern` identifying `qc_names`
#'   of flags with additive effect.
#' @param no_messages A logical value.
#'
#' @seealso [combn_QC()], [ggplot2::ggplot()].
#'
#' @examples
#' set.seed(6)
#' aa <- as.data.frame(replicate(
#' 6, sample(c(0:2, NA), 20, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05))))
#' names(aa) <- letters[1:6]
#'
#' summary_QC(aa, letters[1:6])
#' summary_QC(aa, letters[1:6], na.as = c(NA, 0, NA, NA, NA, NA))
#' summary_QC(aa, letters[1:6], cumul = TRUE, additive = TRUE)
#' summary_QC(aa, letters[1:6], cumul = TRUE, additive = FALSE)
#' is_add <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
#' summary_QC(aa, letters[1:6], cumul = TRUE, additive = is_add)
#'
#' library(ggplot2)
#' (xx <- summary_QC(aa, letters[1:6], cumul = TRUE, plot = TRUE,
#' flux = "CO2 flux"))
#' xx + theme(text = element_text(size = 20))
#'
#' @export
summary_QC <- function(x, qc_names, cumul = FALSE, plot = FALSE, perc = TRUE,
                       flux = NULL, na.as = NULL,
                       na.as_0_pattern = "spikesLF$|fetch70$|man$",
                       additive = NULL, additive_pattern = "interdep$|wresid$",
                       no_messages = FALSE) {
  x_names <- colnames(x)
  qc_names <- as.character(qc_names)
  if (length(qc_names) == 0) return(NULL)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!all(qc_names %in% x_names)) {
    stop(paste("missing", paste0(qc_names[!(qc_names %in% x_names)],
                                 collapse = ", ")))
  }
  if (is.null(na.as)) {
    na.as <- rep(NA, length(qc_names))
    na.as_0 <- grep(na.as_0_pattern, qc_names)
    na.as[na.as_0] <- 0L
    if (!no_messages) {
      if (length(na.as_0)) {
        message("detected columns with 'na.as = 0': ",
                paste0(qc_names[na.as_0], collapse = ", "))
      } else message("no columns with 'na.as = 0' detected")
    }
  } else {
    if (length(na.as) == 0 || (!is.numeric(na.as) && !all(is.na(na.as)))) {
      stop("'na.as' must be a vector containing numeric or NA values")
    }
  }
  if (length(na.as) > 1) {
    if (length(qc_names) != length(na.as)) {
      stop("'na.as' must be of same lenght as 'qc_names' or length 1")
    }
  } else if (length(na.as) != length(qc_names)) {
    na.as <- rep(na.as, length(qc_names))
  }
  # Explicit row subsetting [1:nrow(x)] to strip varnames and units attributes
  # - reshape2::melt() otherwise throws warning if they differ among QC filters
  df <- x[1:nrow(x), qc_names, drop = FALSE]
  if (any(df > 2, na.rm = TRUE) || any(df < 0, na.rm = TRUE))
    stop("QC flags must be within range 0 - 2")
  if (!all(is.na(na.as))) {
    for (i in seq_along(qc_names)) {
      df[is.na(df[i]), i] <- na.as[i]
    }
  }
  # for cumul = TRUE & length(qc_names) == 1 apply() drops dimensions which
  # causes problems during plotting (transposition does not revert to columns)
  # - this can be avoided as cumulative effect is observable only if
  #   length(qc_names) > 1
  if (cumul && length(qc_names) > 1) {
    if (is.null(additive)) {
      additive <- grepl(additive_pattern, qc_names)
      if (!no_messages) {
        if (sum(additive)) {
          message("detected columns with additive effect: ",
                  paste0(qc_names[additive], collapse = ", "))
        } else message("no columns with additive effect detected")
      }
    } else {
      if (!is.logical(additive) || anyNA(additive) || length(additive) == 0) {
        stop("'additive' must be logical vector with non-missing values")
      }
    }
    if (length(additive) > 1) {
      if (length(qc_names) != length(additive)) {
        stop("'additive' must be of same lenght as 'qc_names' or length 1")
      }
    }
    if (all(additive)) {
      df <- as.data.frame(t(apply(df, 1, cumsum)))
    } else if (!any(additive)) {
      df <- as.data.frame(t(apply(df, 1, cummax)))
    } else {
      tmp <- df
      tmp[2:ncol(df)] <- NA
      for (i in 2:ncol(df)) {
        tmp[i] <- if (additive[i]) {
          rowSums(cbind(tmp[i - 1], df[i]))
        } else {
          apply(cbind(tmp[i - 1], df[i]), 1, max)
        }
      }
      df <- tmp
    }
    df[df > 2] <- 2
  }
  df_m <- reshape2::melt(df, id.vars = NULL, variable.name = "QC_filter",
                        value.name = "QC_flag")
  df_m$QC_flag <- as.factor(paste0("flag_", df_m$QC_flag))
  if (perc) {
    tab <- round(table(df_m, useNA = "ifany") / (nrow(df) / 100), 1)
  } else {
    tab <- table(df_m, useNA = "ifany")
  }
  if (!plot) {
    return(tab)
  } else {
    tab_m <- reshape2::melt(tab, variable.name = "QC_filter")
    title <- ifelse(cumul,
                    paste0(c(flux, "Cumulative effect of QC flags"),
                           collapse = " - "),
                    paste0(c(flux, "Independent QC flags"),
                           collapse = " - "))
    y_label <- ifelse(perc, "Percentage", "Count")
    ggplot2::ggplot(tab_m) +
      ggplot2::aes(x = .data$QC_filter, y = .data$value, fill = .data$QC_flag) +
      ggplot2::geom_bar(stat = 'identity',
                        position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::scale_fill_hue(guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::labs(title = title, y = y_label) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 20, vjust = 1, hjust = 1))
  }
}

#' Remap Variables
#'
#' Extracts and renames specified columns of a data frame, computes mean in case
#' of [regular expression] pattern matching multiple column names or
#' initializes one if missing.
#'
#' New data frame is created based on `x` and specified `source`.
#' Original `x` names are changed according to respective `new`
#' elements and kept as `varnames` attributes for traceability.
#' Accordingly, if `qc` is specified, quality control (QC) columns are
#' marked by `"qc_"` prefix.
#'
#' `qc` is specified as the character string pattern that distinguishes QC
#' columns from the actual respective variables. Ideally, prefix should be used
#' for QC columns. E.g. in the case of `"var"` and `"qcode_var"`,
#' `qc = "qcode_"`. QC column can be also marked by suffix. E.g. in the
#' case of `"var_qcode"`, `qc = "_qcode"`. The atypical case of QC
#' marked by both prefix and suffix can be handled too. E.g. in the case of
#' `"prefix_var_suffix"`, `qc = "prefix_|_suffix"`. In case of other
#' exceptions, `new` and `source` can be used to define the QC
#' remapping explicitly.
#'
#' If `regexp = FALSE` (the default), strictly one variable (column) will
#' be remapped to new name. The `source` elements must exactly match
#' `x` names, otherwise expected column is initialized with `NA`s. If
#' `qc` is specified, strictly one respective quality control column will
#' be renamed or skipped if not present.
#'
#' If `regexp = TRUE`, multiple columns can match the `source` element
#' [regular expression] pattern. In that case [rowMeans()]
#' are produced and names of averaged columns kept as `varnames` attributes
#' for traceability. Similarly, also quality control flags are averaged over
#' available columns if `qc` is specified. Note that variable names need to
#' have unique patterns in order to achieve expected results. E.g. precipitation
#' abbreviated as *P* will have overlap with PAR; instead, Precip or sumP
#' can be used.
#'
#' `varnames` attribute is expected. If not automatically assigned to
#' `x` through [read_eddy()] when read from a file, they should
#' be assigned before remapping to keep documentation (especially if multiple
#' columns are combined to a single one).
#'
#' @param x data frame
#' @param new A character vector of new column names for remapping.
#' @param source A vector of `x` column names matching `new` to remap.
#'   If `regexp = TRUE`, character vector containing
#'   [regular expression][regexp]s.
#' @param regexp A logical value. If `FALSE` (the default), `source`
#'   will be interpreted literally. If `TRUE`, `source` elements will
#'   be used as [grep()] `pattern`s.
#' @param qc A character string. A [regular expression]
#'   [grep()] `pattern` identifying `x` column names that
#'   carry quality control information for respective `source`.
#' @param na.rm A logical value indicating whether `NA` values should be
#'   stripped before the computation proceeds. `na.rm` is used only if
#'   `regexp = TRUE` and multiple columns identified by `source` are
#'   combined by averaging.
#'
#' @return A data frame with attributes `varnames` and `units`
#'   assigned to each respective column.
#'
#' @seealso [varnames()].
#'
#' @examples
#' # Simulate soil temperature profile at different depths/positions
#' Ts_profile <- data.frame(
#'   timestamp = seq(c(ISOdate(2023,1,1,0,30)), by = "30 mins", length.out = 5)
#'   )
#' head(Ts_profile)
#' set.seed(42) # makes random numbers reproducible
#' cm_0 <- paste0("Ts_0.00_", c("N", "E", "S", "W"))
#' Ts_profile[cm_0] <- data.frame(replicate(4, rnorm(5)))
#' head(Ts_profile)
#' cm_10 <- paste0("Ts_0.10_", c("N", "E", "S", "W"))
#' Ts_profile[cm_10] <- data.frame(replicate(4, rnorm(5, 5)))
#' head(Ts_profile)
#' Ts_profile$Ts_0.20_E <- rnorm(5, 10)
#' head(Ts_profile)
#' Ts_profile[paste0("qc_", c(cm_0, cm_10, "Ts_0.20_E"))] <- 0
#' varnames(Ts_profile) <- names(Ts_profile)
#' str(Ts_profile)
#' Ts_profile <- Ts_profile[sample(varnames(Ts_profile))]
#' head(Ts_profile)
#'
#' # Literal remapping with regexp = FALSE
#' literal_remapping <- data.frame(
#'   orig_varname = c("timestamp", "Ts_0.00_N", "Ts_0.10_N", "Ts_0.20_E"),
#'   renamed_varname = c("TIMESTAMP", "TS_1_1_1", "TS_1_2_1", "TS_2_3_1")
#'   )
#' literal_remapping
#'
#' rmap1 <- remap_vars(Ts_profile,
#'                     literal_remapping$renamed_varname,
#'                     literal_remapping$orig_varname,
#'                     qc = "qc_")
#' str(rmap1)
#'
#' # Remapping based on string patterns with regexp = TRUE
#' regexp_remapping <- data.frame(
#'   orig_varname = c("timestamp", "Ts_0.00", "Ts_0.10", "Ts_0.20"),
#'   renamed_varname = c("TIMESTAMP", "Tsoil_0cm", "Tsoil_10cm", "Tsoil_20cm")
#'   )
#' regexp_remapping
#'
#' rmap2 <- remap_vars(Ts_profile,
#'                     regexp_remapping$renamed_varname,
#'                     regexp_remapping$orig_varname,
#'                     regexp = TRUE,
#'                     qc = "qc_")
#' # Notice that if pattern matches multiple columns, they are averaged
#' str(rmap2)
#'
#' @export
remap_vars <- function(x, new, source, regexp = FALSE, qc = NULL,
                       na.rm = TRUE) {
  if (!is.data.frame(x) || is.null(names(x))) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (anyDuplicated(names(x))) stop("Duplicated colnames in 'x'")
  new <- as.character(new)
  source <- as.character(source)
  if (length(new) != length(source)) stop(
    paste0("length of 'new' (", length(new),
           ") not equal to lenght of 'source' (",
           length(source), ")"))
  if (anyDuplicated(new)) stop("Duplicated 'new' elements")
  names(source) <- new
  regexp <- as.logical(regexp)
  if (length(regexp) != 1 || is.na(regexp)) stop(
    "'regexp' must be 'TRUE' or 'FALSE'")
  if (!is.null(qc)) {
    qc <- as.character(qc)
    if (length(qc) != 1) stop("'qc' must be a character string of length 1")
  }
  out <- x[, 0]
  if (regexp) {
    qc_index <- if (is.null(qc)) rep(FALSE, length(names(x))) else
      seq_along(names(x)) %in% grep(qc, names(x))
    for (i in new) {
      index <- seq_along(names(x)) %in% grep(source[i], names(x))
      if (sum(!qc_index & index) >= 2) { # if multiple variables, no QC check
        first <- which(!qc_index & index)[1]
        temp <- rowMeans(x[!qc_index & index], na.rm = na.rm)
        varnames(temp) <-
          paste0("mean(",
                 paste(varnames(x[!qc_index & index]), collapse = ", "),
                 ", na.rm = ", na.rm, ")")
        units(temp) <- units(x[first])
        out[i] <- temp
        # check if QC defined and present, otherwise skip
        if (!is.null(qc) && sum(qc_index & index)) {
          first_qc <- which(qc_index & index)[1]
          temp_qc <- rowMeans(x[qc_index & index], na.rm = na.rm)
          varnames(temp_qc) <-
            paste0("mean(",
                   paste(varnames(x[qc_index & index]), collapse = ", "),
                   ", na.rm = ", na.rm, ")")
          units(temp_qc) <- units(x[first_qc])
          out[paste0("qc_", i)] <- temp_qc
        }
      } else if (sum(!qc_index & index) == 0) { # if no match
        out[i] <- NA
        varnames(out[i]) <- i
        units(out[i]) <- "-"
        cat(sprintf("Column with pattern '%s' not found in 'x'\n",
                    source[i]))
        cat(sprintf("Respective column '%s' initialized with NA values\n", i))
      } else { # if one on one case
        out[i] <- x[!qc_index & index]
        # Stop if multiple QC columns (print them)
        if (ncol(x[qc_index & index]) > 1) {
          stop("multiple QC columns (",
               paste(names(x)[qc_index & index], collapse = ", "),
               ") for single variable (",
               names(x)[!qc_index & index],
               ")")
        }
        out[paste0("qc_", i)] <- x[qc_index & index]
      }
    }
  } else { # if regexp = FALSE
    all_qc <- if (is.null(qc)) FALSE else
      seq_along(names(x)) %in% grep(qc, names(x))
    for (i in new) {
      index <- match(source[i], names(x))
      qc_index <- if (is.null(qc)) FALSE else
        match(source[i], gsub(qc, "", names(x)[all_qc]), nomatch = 0)
      if (is.na(index)) {
        out[i] <- NA
        varnames(out[i]) <- i
        units(out[i]) <- "-"
        cat(sprintf("Column '%s' not found in 'x'\n", source[i]))
        cat(sprintf("Respective column '%s' initialized with NA values\n", i))
      } else {
        out[i] <- x[index]
        if (!is.null(qc)) out[paste0("qc_", i)] <- x[all_qc][qc_index]
      }
    }
  }
  return(out)
}

#' Merge Regular Date-Time Sequence and Data Frames
#'
#' Merge generated regular date-time sequence (timestamp) with single or
#' multiple data frames containing timestamp.
#'
#' The primary purpose of `merge_eddy()` is to combine chunks of data vertically
#' along their column `"timestamp"` with date-time information. This timestamp
#' is expected to be regular with given time `interval`. The resulting data
#' frame contains added rows with expected date-time values missing in
#' timestamp, followed by `NA`s across respective rows. In case that
#' `check_dupl = TRUE` and timestamp values across `x` elements overlap,
#' detected duplicated rows are removed (the order in which duplicates are
#' evaluated depends on the order of `x` elements). A special case when `x` has
#' only one element allows to fill missing date-time values in `"timestamp"`
#' column of given data frame.
#'
#' The list of data frames, each with column `"timestamp"`, is sequentially
#' [merge()]d using [Reduce()]. A *(full) outer join*,
#' i.e. `merge(..., all = TRUE)`, is performed to keep all columns of
#' `x` elements. The order of `x` elements can affect the result.
#' Duplicated column names within `x` elements are corrected using
#' [make.unique()]. The merged data frame is then merged on the
#' validated `"timestamp"` column that can be either automatically
#' extracted from `x` or manually specified.
#'
#' For horizontal merging (adding columns instead of rows) `check_dupl =
#' FALSE` must be set but simple [merge()] could be preferred.
#' Combination of vertical and horizontal merging should be avoided as it
#' depends on the order of `x` elements and can lead to row duplication.
#' Instead, data chunks from different data sources should be first separately
#' vertically merged and then merged horizontally in a following step.
#'
#' If `interval = NULL`, automated recognition of `interval` is applied. This is
#' preferred to setting `interval` value manually. Only in rare cases when
#' original time interval is not present in `x` due to gaps, it is not possible
#' to infer the original time interval from the timestamps. The inferred
#' interval represents the shortest time interval present among `x` records.
#' Thus if the expected interval is shorter, it needs to be set manually.
#'
#' The default [`storage.mode`] of `"timestamp"` column is set to be
#' `"integer"` instead of `"double"`. This simplifies the application of
#' [`round_df()`] (it avoids rounding) but could lead to an unexpected behavior
#' if the date-time information is expected to resolve fractional seconds (it
#' [`trunc()`]ates decimals).
#'
#' @param x List of data frames, each with `"timestamp"` column of class
#'   `"POSIXt"`. Optionally with attributes `varnames` and
#'   `units` for each column.
#' @param start,end A value specifying the first (last) value of the generated
#'   date-time sequence. If `NULL`, [min()] ([max()])
#'   is taken across the values in `"timestamp"` columns across `x`
#'   elements. If numeric, the value specifies the year for which the first
#'   (last) date-time value will be generated, considering given time
#'   `interval` and convention of assigning of measured records to the end
#'   of the time interval. Otherwise, character representation of specific half
#'   hour is expected with given `format` and `tz`.
#' @param check_dupl A logical value specifying whether rows with duplicated
#'   date-time values checked across `x` elements should be excluded before
#'   merging.
#' @param interval A numeric value specifying the time interval (in seconds) of
#'   the generated date-time sequence.
#' @param format A character string. Format of `start` (`end`) if
#'   provided as a character string.The default [format][strptime]
#'   is `"%Y-%m-%d %H:%M"`.
#' @param tz A time zone (see [time zones]) specification to be used
#'   for the conversion of `start` (`end`) if provided as a character
#'   string.
#' @param storage.mode A character string. Either `"integer"` (default) or
#'   `"double"` (see Details).
#'
#' @return A data frame with attributes `varnames` and `units` for
#'   each column, containing date-time information in column `"timestamp"`.
#'
#' @seealso [merge()], [Reduce()], [strptime()],
#'   [time zones], [make.unique()]
#'
#' @examples
#' set.seed(123)
#' n <- 20 # number of half-hourly records in one non-leap year
#' tstamp <- seq(c(ISOdate(2021,3,20)), by = "30 mins", length.out = n)
#' x <- data.frame(
#' timestamp = tstamp,
#' H = rf(n, 1, 2, 1),
#' LE = rf(n, 1, 2, 1),
#' qc_flag = sample(c(0:2, NA), n, replace = TRUE)
#' )
#' openeddy::varnames(x) <- c("timestamp", "sensible heat", "latent heat",
#'                            "quality flag")
#' openeddy::units(x) <- c("-", "W m-2", "W m-2", "-")
#' str(x)
#' y1 <- ex(x, 1:10)
#' y2 <- ex(x, 11:20)
#' y <- merge_eddy(list(y1, y2))
#' str(y)
#' attributes(y$timestamp)
#' typeof(y$timestamp)
#'
#' # Duplicated rows and different number of columns
#' z1 <- ex(x, 8:20, 1:3)
#' z <- merge_eddy(list(y1, z1))
#'
#' @importFrom utils relist
#' @export
merge_eddy <- function(x, start = NULL, end = NULL, check_dupl = TRUE,
                       interval = NULL, format = "%Y-%m-%d %H:%M", tz = "GMT",
                       storage.mode = "integer") {
  sq <- seq_len(length(x))
  check_x <- lapply(x, function(x) any(!is.data.frame(x),
                                       !inherits(x$timestamp, "POSIXt")))
  if (any(unlist(check_x)))
    stop(strwrap("'x' must be a list of data frames with 'timestamp'
         column of POSIXt class", prefix = " ", initial = ""))
  if (any(sapply(x, function(x) anyNA(x$timestamp))))
    stop("'timestamp' includes NA value(s)")
  # col dups must be treated within each list element
  if (any(unlist(lapply(x, function(x) duplicated(names(x)))))) {
    warning("Duplicated names in 'x' elements: corrected by 'make.unique()'")
    for (i in sq) names(x[[i]]) <- make.unique(names(x[[i]]))
  }
  # POSIXlt causes problems/errors during duplication check and merging
  is_POSIXlt <- unlist(lapply(x, function(x) "POSIXlt" %in% class(x$timestamp)))
  if (any(unlist(is_POSIXlt))) {
    for (i in sq) {
      if (!is_POSIXlt[i]) next
      v <- varnames(x[[i]], names = TRUE)
      u <- units(x[[i]], names = TRUE)
      x[[i]]$timestamp <- as.POSIXct(x[[i]]$timestamp)
      varnames(x[[i]]) <- v
      units(x[[i]]) <- u
    }
  }
  # check if x has duplicated rows
  # treatment is optional and done across elements before merging
  if (check_dupl) {
    # x elements are reduced from data frames to vectors (required by relist)
    ts <- lapply(x, function(x) x$timestamp)
    # identify position (row) with duplicated timestamp across data frames
    dupl <- duplicated(unlist(ts)) # conversion from POSIXt to integer is OK
    if (any(dupl)) {
      # relist the dupl vector so it can be matched against the original list 'x'
      l_dupl <- relist(dupl, ts)
      xrows <- vector("character", length(x))
      for (i in sq) {
        if (any(l_dupl[[i]])) {
          xrows[i] <- paste0("x[[", i, "]]: row(s) ",
                             paste(which(l_dupl[[i]]), collapse = ", "),
                             "\n")
        } else next
      }
      message("removing rows in 'x' elements with duplicated timestamp at:\n",
              xrows)
      # remove the duplicated rows from the elements of 'x'
      for (i in sq) x[[i]] <- ex(x[[i]], !l_dupl[[i]], )
    }
  }
  # handle single data frame (timestamp correction applied)
  if (length(x) == 1L) {
    out <- x[[1]]
    out_vu <- as.data.frame(do.call(
      rbind,
      list(varnames(x[[1]], names = TRUE), units(x[[1]], names = TRUE))))
  } else {
    # normal case of merging multiple data frames in list 'x'
    out <- Reduce(function(x, y)
      merge(x, y, by = intersect(names(x), names(y)), all = TRUE), x)

    # extract variables and units and bind within each list element as data frame
    vu <- lapply(x, function(x) as.data.frame(do.call(
      rbind,
      list(varnames(x, names = TRUE), units(x, names = TRUE)))))

    # merge to get the same order and number of variables as in 'out'
    out_vu <- Reduce(function(x, y)
      merge(x, y, by = intersect(names(x), names(y)), all = TRUE, sort = FALSE),
      vu) # 'sort = TRUE' switches order of rows
    # needs to be tested:
    # - merge produces data frame with combinations of varnames and units
    # - first row seems to correspond fully to varnames, last row to units
    out_vu <- out_vu[c(1, nrow(out_vu)), ]
  }
  range <- range(out$timestamp)



  # test above code formally and correct code below (use infer_interval(), tdiff())
  if (is.null(interval)) {
    # automated estimation of interval
    # working on list is more reliable due to possible gaps among its elements
    interval <- median(do.call(c, lapply(x, function(x) diff(x$timestamp))))  #### use infer_interval() + further checks from strptime_eddy()
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- as.difftime(interval, units = "secs")
  }
  if (diff(range) < interval) # this should be removed?? -------------------------------
    stop("'interval' is larger than 'timestamp' range")
  # For both start and end arguments:
  # if NULL - get value automatically from x input
  # if numeric - the value represents start/end of given year
  # otherwise expect character representation of specific half hour
  if (is.null(start)) {
    start <- range[1]
  } else if (is.numeric(start)) {
    start <- ISOdate(start, 1, 1, 0) + as.numeric(interval, units = "secs")
  } else {
    start <- strptime(start, format = format, tz = tz)
  }
  if (is.null(end)) {
    end <- range[2]
  } else if (is.numeric(end)) {
    end <- ISOdatetime(end + 1, 1, 1, 0, 0, 0, tz = tz)
  } else {
    end <- strptime(end, format = format, tz = tz)
  }
  # seq.POSIXt converts to POSIXct so strptime POSIXlt product does not matter
  # timestamp should not have missing values or reversed order
    if (start > end) stop("generated 'timestamp' would have reversed order")
  full_ts <- data.frame(timestamp = seq(start, end, by = interval))
  # It is not possible to reduce both EP and full_ts in one step
  # First step with Reduce aims to keep all rows and columns of 'x' data frames
  # Second step trims them according to the specified timestamp range (all.x)
  out <- merge(full_ts, out, by = "timestamp", all.x = TRUE)
  # Is resulting time series regular? Tested independently on check_dupl
  if (length(unique(diff(out$timestamp))) > 1) {
    warning("resulting timestamp does not form regular sequence")
  }
  # Last merge could move timestamp so names need to be matched
  pos <- match(names(out), names(out_vu))
  varnames(out) <- t(out_vu)[pos, 1] # t() to extract as vector
  units(out) <- t(out_vu)[pos, 2]
  # Set storage mode of timestamp to integer to simplify data frame rounding
  storage.mode(out$timestamp) <- storage.mode
  return(out)
}

#' Read Meteorological Data with Units
#'
#' Read single or multiple meteorological data files at Czechglobe MeteoDBS
#' format at given path and merge them together along generated regular
#' date-time sequence.
#'
#' This utility function is adapted to Czechglobe MeteoDBS file structure but
#' allows to change selected useful arguments that have preset default values.
#' It also assures that date-time sequence is regular and equidistant.
#'
#' In case that multiple files are present in the `path`, the expectation is
#' that files represent meteorological variables for given site and different
#' periods. Function merges them vertically (along generated complete
#' timestamp). All original columns across all files excluding the last empty
#' one are kept. The order of variables keeps that of the first file loaded
#' (note that file ordering in `path` is alphabetical not chronological) and
#' additional variables are appended if present in the following files. The
#' output "date/time" column is converted into class `POSIXct`.
#'
#' If you want to specify `start` and `end` arguments as strings and you change
#' also default `shift.by` value, `start` and `end` arguments need to be adopted
#' accordingly to account for that change. E.g. if `shift.by = -900`, then
#' `start = "2019-12-31 21:15:00", end = "2019-12-31 23:15:00"` instead of
#' `start = "2019-12-31 21:30:00", end = "2019-12-31 23:30:00"` for half-hourly
#' data.
#'
#' Function introduces additional column "timestamp" for purposes of merging
#' with `merge_eddy()`. This column is then removed as it is not included in the
#' original data.
#'
#' @return A data frame is produced with additional attributes `varnames` and
#'   `units` assigned to each respective column.
#'
#' @param path A string. The path to directory with CSV file(s) in Czechglobe
#'   MeteoDBS format. Other than CSV files are ignored.
#' @param start,end A value specifying the first (last) value of the generated
#'   date-time sequence in temporary column "timestamp". If `NULL`, [min()]
#'   ([max()]) of date-time values from "date/time" column across all files is
#'   used. If numeric, the value specifies the year for which the first (last)
#'   date-time value will be generated, considering given time interval
#'   (automatically detected from "date/time" column) and convention of
#'   assigning of measured records to the end of the time interval. Otherwise,
#'   character representation of specific date-time value is expected in given
#'   `format` and timezone "GMT".
#' @param format A character string. Format of `start` (`end`) if provided as a
#'   character string.
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If `TRUE`, date-time information does not
#'   have to be regular but time differences must be multiples of automatically
#'   detected time interval.
#' @param verbose A logical value. Should additional statistics about presence
#'   of `NA` values in resulting data frame be printed to console?
#' @param pattern A character string. A [regular expression][regexp] [grep()]
#' `pattern` identifying MeteoDBS files in the `path` folder.
#'
#' @examples
#' # examples of different patterns for file selection
#' xx <- c("CZ-BK1_2024_meteo.csv", "data.CSV", "CZ-BK1.txt")
#'
#' # select file names ending with ".csv" (case insensitive)
#' grep("\\.[Cc][Ss][Vv]$", xx, value = TRUE)
#'
#' # select file names starting with CZ-BK1 site abbreviation
#' grep("^CZ-BK1", xx, value = TRUE)
#'
#' # select CSV file names starting with CZ-BK1 site abbreviation
#' # - note the usage of ".*" to combine above patterns
#' grep("^CZ-BK1.*\\.[Cc][Ss][Vv]$", xx, value = TRUE)
#'
#' @importFrom utils read.table
#' @export
read_MeteoDBS <- function(path, start = NULL, end = NULL,
                          format = "%d.%m.%Y %H:%M", shift.by = NULL,
                          allow_gaps = TRUE, verbose = TRUE,
                          pattern = "\\.[Cc][Ss][Vv]$") {
  lf <- list.files(path, full.names = TRUE)
  lf <- grep(pattern, lf, value = TRUE) # "\\." is literal dot
  if (length(lf) == 0) stop("no file matching 'pattern' in folder 'path'")
  l <- vector("list", length(lf))
  names(l) <- lf
  for (i in seq_along(lf)) {
    # Meteo data have header on line 10 and units on line 12 (remove line 11)
    l[[i]] <- readLines(lf[i])[-11]
    # Write modified Meteo data to temporary file
    file <- tempfile()
    writeLines(l[[i]], file)
    # Load Meteo data with units and remove the temporary file
    l[[i]] <- read_eddy(file, skip = 9, sep = ";", check.names = FALSE)
    unlink(file)
    # All files downloaded from MeteoDBS include empty last column without name
    # This complicates merging and should be removed
    last_col <- ncol(l[[i]])
    if (names(l[[i]])[last_col] == "" && all(is.na(l[[i]][, last_col])))
      l[[i]][last_col] <- NULL
    # Create "timestamp" of class "POSIXct" required by merge_eddy()
    l[[i]]$timestamp <-
      strptime_eddy(l[[i]]$`date/time`, format, shift.by = shift.by,
                    allow_gaps = allow_gaps)
  }
  # Merge all chunks together
  res <- merge_eddy(l, start, end)
  message("if present, all gaps in timestamp were filled")
  # Overwrite original "date/time" by corrected (validated) "timestamp"
  res$`date/time` <- res$timestamp
  # Remove "timestamp" column that was not included in original data
  res$timestamp <- NULL
  # strptime_eddy() automatically sets "date/time" varname to "timestamp"
  varnames(res)[names(res) == "date/time"] <- "date/time"
  # Report stats about NA values in resulting data frame
  if (verbose) {
    # computations on data frames are highly inefficient - convert to matrix
    mres <- data.matrix(res) # Converts also characters
    NA_tot <- sum(is.na(mres))
    # Continue evaluation only if any value missing
    # - computation can take few secs so moved above any printed text
    if (NA_tot) {
      # Find row indices without any meteorological data
      i_NA_rows <- allNA(
        mres[, !(colnames(mres) %in% "date/time"), drop = FALSE], 1)
      # Find columns without any meteorological data
      NA_cols <- allNA(mres, 2) # ("date/time" checked too)
      # Find columns with gaps in meteorological data
      gaps <- apply(mres, 2, anyNA)
    }
    cat("Total number of missing values: ", NA_tot, "\n", sep = "")
    # Continue evaluation only if any value missing
    if (NA_tot) {
      sum_NA_rows <- sum(i_NA_rows)
      cat("Rows without any meteorological data: ")
      if (!sum_NA_rows) {
        cat("none\n")
      } else {
        cat(sum_NA_rows, " \n")
        # Make differences across logical vector indicating if all values are NA
        # To check also first and last row 0 value is added at the beginning and end
        allNA_diff <- c(diff(c(0, i_NA_rows, 0)))
        # allNA_diff == 1 gives indices where gaps start (including those indices)
        s <- which(allNA_diff == 1)
        # allNA_diff == -1 gives indices where gaps end (excluding those indices)
        e <- which(allNA_diff == -1) # value -1 gives index+1 after which gap ends
        # Timestamps represent the end of measurement interval, therefore it should
        # be shifted (timestamp preceding the first record is included)
        interval <- median(diff(res$`date/time`))
        ts <- c(res$`date/time`[1] - interval, res$`date/time`)
        info_rows <- data.frame(paste0("(", ts[s], ","),
                                paste0(ts[e], "]"))
        info_rows <- as.matrix(format(info_rows))
        dimnames(info_rows) <- list(rep("", nrow(info_rows)),
                                    c("gap start ", "gap end "))
        print(info_rows, quote=FALSE, right=TRUE)
        cat("- missing rows account for",
            sum_NA_rows * (ncol(res) - 1), # number of cols excluding timestamp
            "missing values\n")
      }
      cat("Columns without any meteorological data: ")
      sum_NA_cols <- sum(NA_cols)
      if (!sum_NA_cols) {
        cat("none\n")
      } else {
        cat(sum_NA_cols, " \n- missing column names:\n")
        # Quoted names are useful because they can be used for extraction
        op <- options("useFancyQuotes")
        options(useFancyQuotes = FALSE)
        cat(sQuote(names(NA_cols[NA_cols == TRUE])), sep = ", ", fill = TRUE)
        options(op)
        cat("- missing columns account for", sum_NA_cols * nrow(res),
            "missing values\n")
      }
      # If at least one row is missing checking for gaps across cols is not useful
      if (!sum_NA_rows) {
        sum_gaps <- sum(gaps)
        cat("Columns with gaps: ")
        if (!sum_gaps) {
          cat("none\n")
        } else {
          cat(sum_gaps, " \n- names of columns with gaps:\n")
          # Quoted names are useful because they can be used for extraction
          op <- options("useFancyQuotes")
          options(useFancyQuotes = FALSE)
          cat(sQuote(names(gaps[gaps == TRUE])), sep = ", ", fill = TRUE)
          options(op)
        }
      }
    }
  }
  return(res)
}

#' Read EddyPro Files with Units
#'
#' Read single or multiple EddyPro full output files at given path and merge
#' them together along generated regular date-time sequence.
#'
#' This utility function is adapted to EddyPro full output file structure but
#' allows to change selected useful arguments that have preset default values.
#' Column "timestamp" with date-time information is constructed based on "date"
#' and "time" columns and converted into class `POSIXct`. It also assures
#' that date-time sequence is regular and equidistant.
#'
#' In case that multiple files are present in the `path`, function merges
#' them vertically (along generated complete timestamp) and discards rows with
#' duplicated date-time values. All original columns across all files are kept.
#' The order of variables follows that of the first file loaded (note that file
#' ordering in `path` is alphabetical not chronological) and additional
#' variables are appended if present in the following files. To assure
#' compatibility with older EddyPro versions, old column name "max_speed" is
#' renamed to "max_wind_speed" if present.
#'
#' If you want to specify `start` and `end` arguments as strings and
#' you change also default `shift.by` value, `start` and `end`
#' arguments need to be adopted accordingly to account for that change. E.g. if
#' `shift.by = -900`, then `start = "2019-12-31 21:15:00", end =
#' "2019-12-31 23:15:00"` instead of `start = "2019-12-31 21:30:00", end =
#' "2019-12-31 23:30:00"` for half-hourly data.
#'
#' Note that `skip` and `fileEncoding` arguments must be valid across
#' all files, otherwise the function will not execute correctly.
#'
#' @return A data frame is produced with additional attributes `varnames`
#'   and `units` assigned to each respective column.
#'
#' @param path A string. The path to directory with EddyPro full output. Other
#'   than CSV files are ignored.
#' @param start,end A value specifying the first (last) value of the column
#'   "timestamp" in outputted data frame. If `NULL`, [min()]
#'   ([max()]) of date-time values from "timestamp" column across all
#'   input files is used. If numeric, the value specifies the year for which the
#'   first (last) date-time value will be generated, considering given time
#'   interval (automatically detected from "timestamp" column) and convention of
#'   assigning of measured records to the end of the time interval. Otherwise,
#'   character representation of specific date-time value is expected in given
#'   `format` and timezone "GMT".
#' @param skip An integer. The number of lines to skip in the input file before
#'   reading data.
#' @param fileEncoding A character string. If non-empty, declares the encoding
#'   used on a file (not a connection) so the character data can be re-encoded.
#'   See [read.table()] for further details.
#' @param format A character string. Format of `start` (`end`) if
#'   provided as a character string.
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If `TRUE`, date-time information does
#'   not have to be regular but time differences must be multiples of
#'   automatically detected time interval.
#' @param pattern A character string. A [regular expression][regexp] [grep()]
#' `pattern` identifying EddyPro full output files in the `path` folder.
#'
#' @examples
#' # examples of different patterns for file selection
#' xx <- c("CZ-BK1_2024_eddy.csv", "data.CSV", "CZ-BK1.txt")
#'
#' # select file names ending with ".csv" (case insensitive)
#' grep("\\.[Cc][Ss][Vv]$", xx, value = TRUE)
#'
#' # select file names starting with CZ-BK1 site abbreviation
#' grep("^CZ-BK1", xx, value = TRUE)
#'
#' # select CSV file names starting with CZ-BK1 site abbreviation
#' # - note the usage of ".*" to combine above patterns
#' grep("^CZ-BK1.*\\.[Cc][Ss][Vv]$", xx, value = TRUE)
#'
#'
#' @export
read_EddyPro <- function(path, start = NULL, end = NULL, skip = 1,
                         fileEncoding = "UTF-8", format = "%Y-%m-%d %H:%M",
                         shift.by = NULL, allow_gaps = TRUE,
                         pattern = "\\.[Cc][Ss][Vv]$") {
  lf <- list.files(path, full.names = TRUE)
  lf <- grep(pattern, lf, value = TRUE) # "\\." is a literal dot
  if (length(lf) == 0) stop("no file matching 'pattern' in folder 'path'")
  l <- vector("list", length(lf))
  names(l) <- lf
  for (i in seq_along(lf)) {
    l[[i]] <- read_eddy(lf[i], check.names = FALSE, skip = skip,
                        fileEncoding = fileEncoding)
    timestamp <- strptime_eddy(paste(l[[i]]$date, l[[i]]$time),
                               format = format, shift.by = shift.by,
                               allow_gaps = allow_gaps)
    l[[i]] <- cbind(timestamp, l[[i]])
    # exception1: rename old column name "max_speed" to new "max_wind_speed"
    exception1 <- grepl("max_speed", names(l[[i]]))
    if (sum(exception1)) {
      names(l[[i]])[exception1] <- "max_wind_speed"
      message("column name 'max_speed' renamed to 'max_wind_speed' in ", lf[i])
    }
  }
  EP <- merge_eddy(l, start, end)
  message("if present, all gaps in timestamp were filled")
  return(EP)
}

#' Strip Positional Qualifier Suffix
#'
#' Function removes from variable name the suffix with three indices
#' representing horizontal and vertical placement and number of replicates
#' (_H_V_R suffix used in tower network naming strategy).
#'
#' If `warn = TRUE`, it is checked if multiple _H_V_R suffixes were
#' detected. This might be undesired based on the application.
#'
#' @param x A string vector.
#' @param warn A logical value
#'
#' @return A string vector with extracted variable codes.
#'
#' @examples
#' x <- c("TA_1_1_1", "TS_1_1_1", "VPD")
#' strip_suffix(x)
#'
#' @export
strip_suffix <- function(x, warn = FALSE) {
  x <- as.character(x)
  # FLUXNET (and ICOS) naming strategy VAR_H_V_R
  # VAR = is the official variable code , only alphabetic characters and
  #       underscores, all capital
  # H = horizontal position index, integer number
  # V = vertical position index, integer number
  # R = replicate index, integer number

  # here "_H_V_R" suffix is removed
  vars <- gsub("_[[:digit:]]+_[[:digit:]]+_[[:digit:]]+$", "", x)
  pattern <- paste(vars, collapse = "|")
  suff <- unique(gsub(pattern, "", x))
  suff <- suff[!suff == ""]
  if (warn & length(suff) > 1)
    warning("multiple _H_V_R suffixes detected: ", paste(suff, collapse = ", "))
  return(vars)
}

#' Choose Available Names from Set
#'
#' Available variable names are checked against the full set of expected
#' variables and missing cases are reported.
#'
#' @param names A character vector with available names.
#' @param all_names A character vector with all expected variable names.
#' @param show_ignored A logical value. Should ignored names be shown?
#' @param warn A logical value. Should you be warned if `names` or
#'   `all_names` contain duplicates?
#'
#' @return A character vector with subset of expected variable names.
#'
#' @examples
#' # all available names to choose from
#' all_names <- c("TA", "TS", "VPD", "LE", "H", "NEE")
#'
#' # names to choose if present in all_names
#' # - "PM10" wanted but not available - thus reported as not available
#' # - c("TA", "TS", "VPD", "NEE") available but not wanted, thus ignored
#' names <- c("H", "LE", "PM10")
#' choose_avail(names, all_names)
#'
#' @export
choose_avail <- function(names, all_names, show_ignored = FALSE, warn = TRUE) {
  names <- na.omit(names)
  chosen <- names[names %in% all_names]
  not_avail <- setdiff(names, chosen)
  ignored <- setdiff(all_names, chosen)
  if (warn) {
    if (any(duplicated(names))) {
      warning("duplicated names in 'names'", call. = FALSE)
    }
    if (any(duplicated(all_names))) {
      warning("duplicated names in 'all_names'", call. = FALSE)
    }
  }
  if (length(not_avail))
    message("Following names are not available:\n",
            paste(not_avail, collapse = ", "))
  if (show_ignored && length(ignored))
    message("Following names were ignored:\n",
            paste(ignored, collapse = ", "))
  return(chosen)
}

