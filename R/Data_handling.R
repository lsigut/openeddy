#' Folder Structure Setup
#'
#' Folder structure recommended for eddy covariance data processing and
#' archivation.
#'
#' The purpose is to standardize the locations for metadata and post-processing
#' inputs required to run the proposed workflow
#' (\url{https://github.com/lsigut/EC_workflow}) as well as to store data and
#' metadata in levels corresponding to processing stage. The folder structure is
#' not required to succesfully apply the workflow but simplifies its use.
#'
#' Data processing stages \itemize{\item Level 0: Raw files with measured high
#' frequency eddy covariance data and relevant metadata or instrument setup
#' files. \item Level 1: Processing software setup and output files and a
#' logbook. \item Level 2: Quality checking results and documentation,
#' definition of ecosystem boundary, storage flux processing and files used as
#' inputs for Level 3 data. \item Level 3: gap-filling output and its
#' documentation, summary of the computed fluxes and meteorological data
#' including their aggregation.}
#'
#' @param root A character string defining the root of created folder structure.
#' @param create_dirs A logical value. Indicates whether directories should be
#'   created.
#' @param fsep A character. The path separator to use (assumed to be ASCII).
#' @param ... Further arguments to be passed to \code{dir.create} function.
#'
#' @return A named list with paths to folder structure directories.
#'   Corresponding directories are created as a function side effect if
#'   \code{create_dirs = TRUE}.
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
#' Round the columns of numeric mode type double to specified (default = 6)
#' significant digits.
#'
#' Actual \code{\link{signif}} function is used for rounding. Note that other
#' classes might be internally stored as numeric types. Particularly
#' \code{\link{POSIXct}} class is by default stored as integer (rounding does
#' not apply) but in case of adding (subtracting) double or if displaying
#' fractional seconds, such date-time information will be internally converted
#' to the type double (rounding applies). See examples.
#'
#' @param x A data frame.
#' @param digits An integer. See \code{\link{signif}} for details.
#'
#' @return A data frame with \code{varnames} and \code{units} attributes.
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
#' \code{allNA} returns a logical vector or array or list indicating whether
#' there are only \code{NA} values in selected margins and therefore e.g.
#' statistics like \code{max} or \code{min} do not produce useful results.
#' @param x An array, including a matrix.
#' @param margin A vector giving the subscripts which the function will be
#'   applied over. E.g., for a matrix \code{1} indicates rows, \code{2}
#'   indicates columns, \code{c(1, 2)} indicates rows and columns. Where
#'   \code{x} has named dimnames, it can be a character vector selecting
#'   dimension names.
#' @family NA handlers
#' @seealso \code{\link{NA}} for general information about NAs and
#'   \code{\link{apply}} for \code{apply} description.
#' @examples
#' \dontrun{
#' xx <- matrix(1:20, nrow = 4)
#' xx[2, ] <- NA
#' allNA(xx, 2) # All columns have at least one non-missing value
#' allNA(xx, 1) # Second row has all values missing
#' apply(xx, 1, max, na.rm = TRUE)
#' ## returns c(17, -Inf, 19, 20) and a warning message
#' ## Skip the allNA row in apply()
#' apply(xx[!allNA(xx, 1), ], 1, max, na.rm = TRUE)
#' }
#' @keywords internal
allNA <- function(x, margin) {
  apply(x, margin, function(x) all(is.na(x)))
}

#' Object Attributes Varnames and Units
#'
#' \code{varnames} and \code{units} are useful attributes that can store
#' original variable names (\code{varnames}) and units of measurement
#' (\code{units}) of each column in a data frame or of an atomic type. These
#' attributes can be extracted or assigned by following functions.
#'
#' Functions check whether the extracted or assigned attributes contain elements
#' with \code{NULL}, \code{NA}, \code{""} values or if length of each element is
#' higher than \code{1}. In these cases, such elements are substituted with
#' \code{"-"}.
#'
#' @return For \code{varnames} and \code{units}, a character vector.
#'
#'   For \code{varnames<-} and \code{units<-}, the updated object \code{x}.
#'
#' @param x A data frame or an atomic type.
#' @param names A logical value. Applies only in case of data frames. If
#'   \code{TRUE}, attributes are extracted with corresponding column names.
#' @param value An atomic type that represents \code{varnames} or \code{units}.
#'   The length must be \code{1} if \code{x} is an atomic type or equal to
#'   number of columns in \code{x} if \code{x} is a data frame.
#'
#' @seealso \code{\link{read_eddy}} and \code{\link{write_eddy}}.
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
#' Conserves \code{varnames} and \code{units} attributes of vectors and data
#' frames during extraction.
#'
#' Extraction from atomic types is done as \code{x[i]} ignoring \code{j} and
#' \code{drop} (applies also to matrices and arrays). Extraction from data
#' frames is done as \code{x[i, j, drop]}.
#'
#' @return A vector or data frame with \code{varnames} and \code{units}
#'   attributes.
#'
#' @param x An atomic type or a data frame. Object from which to extract
#'   element(s).
#' @param i,j Indices specifying elements to extract as specified in
#'   \code{\link{Extract}}.
#' @param drop A logical value. If \code{TRUE} (default), the result is coerced
#'   to the lowest possible dimension.
#'
#' @seealso \code{\link{Extract}}, \code{\link{drop}} and
#'   \code{\link{varnames}}.
#'
#' @export
ex <- function(x, i, j, drop = TRUE) {
  v <- varnames(x, names = TRUE)
  u <- units(x, names = TRUE)

  if (is.atomic(x)) {
    out <- x[i]
  } else if (is.data.frame(x)) {
    out <- x[i, j, drop = drop]
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
#' \code{varnames} (representing variable names) and \code{units} (representing
#' units of measurement or space efficient metadata) are assigned to each
#' column.
#'
#' \code{read_eddy} extends the possibilities of \code{\link{read.table}} so it
#' can also read units of measurement. However, it uses default arguments of
#' \code{\link{read.csv}} to accomodate loading of data for the most common
#' input type. \code{read_eddy} also sets useful defaults common for eddy
#' covariance (\emph{eddy}) data. Missing values are often reported as
#' \code{"-9999.0"} or \code{"-9999"} by post-processing software, therefore
#' \code{na.strings = c("NA", "-9999.0", "-9999")} is used as default.
#'
#' Attribute \code{varnames} contains original variable name of respective
#' column without automated conversion that is done for column name. The main
#' purpose of \code{varnames} attribute is to provide control over conversion of
#' original column names and keep variable name of a vector when it is separated
#' from the original data frame.
#'
#' Units are expected to be one line below the header in the input file. Instead
#' of units of measurement, it is possible to include any space efficient
#' metadata that is relevant to the respective variables. E.g. format of
#' timestamp or structure of coded variable. One line below units and further in
#' the input file is the region with data. Any missing values or blank fields
#' (converted to empty strings) in the line interpreted as units will be
#' substituted by \code{units_fill} string instead.
#'
#' The automated check for \code{"-10000"} values in the data region is provided
#' by \code{check_input = TRUE} (default) and produces error message if the
#' value is found. The \code{"-10000"} values can be introduced to the dataset
#' by rounding \code{"-9999"} values due to the incorrect file conversion or
#' data manipulation. Using  \code{check_input = FALSE} will skip the check
#' (this could improve the performance for large input files).
#'
#' @return A data frame is produced with additional attributes \code{varnames}
#'   and \code{units} assigned to each respective column.
#'
#' @param file The file name with input data to be read. It can be a file name
#'   inside the current working directory, \emph{relative} or \emph{absolute}
#'   path or \code{\link{connection}}. See \code{\link{read.table}} for more
#'   detailed description. Connections to anonymous file or clipboard are not
#'   allowed. To read from clipboard use \code{"clipboard"} string instead of
#'   connection.
#' @param header A logical value indicating whether the names of variables are
#'   included as the first line of the input file. If \code{FALSE}, column names
#'   and variable names of attribute \code{varnames} will be automatically
#'   generated.
#' @param units A logical value indicating whether the units for respective
#'   variables are included one line above the data region in the input file. If
#'   \code{FALSE}, the \code{units} attribute of each column will be set to
#'   \code{units_fill} string representing missing values.
#' @param sep A character that separates the fields of input. Default separator
#'   for CSV files is \code{","}. See \code{\link{read.table}} for other
#'   options.
#' @param quote A character string that contains the quoting characters.
#' @param dec A character that specifies decimal mark used in the input.
#' @param units_fill A character string that represents missing value of
#'   \code{units} attribute.
#' @param na.strings A character vector of strings representing \code{NA} values
#'   in the input file. Blank fields are also considered to be missing values in
#'   logical, integer, numeric and complex fields.
#' @param colClasses A character vector of classes to be assumed for the columns
#'   and recycled as necessary. See \code{\link{read.table}} for more detailed
#'   description.
#' @param nrows An integer specifying the maximum number of rows to read in.
#'   Negative and other invalid values are ignored.
#' @param skip An integer. The number of lines to skip in the input file before
#'   reading data.
#' @param fill A logical value. If set to \code{TRUE} (default), the rows that
#'   have unequal length will be corrected with blank fields.
#' @param comment.char A character that is interpreted as comment or empty
#'   string to turn off this behaviour.
#' @param check_input A logical value that determines if values in the input
#'   will be checked for erroneous \code{"-10000"} value. If \code{TRUE}
#'   (default), any encountered \code{"-10000"} value in the data will trigger
#'   an error message.
#' @param ... Further arguments to be passed to the internal \code{read.table}
#'   function
#' @seealso \code{\link{read.table}} for information about further arguments
#'   passed to \code{read.table}.
#'
#'   \code{\link{write_eddy}} to save data frame with \code{units} attributes
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

#' Conversion of Regular Date-time Sequence from Character
#'
#' Converts character vector to class \code{"POSIXct"} using
#' \code{\link{strptime}} and validates the result. The input has to represent a
#' regular date-time sequence with given time interval. Additional attributes
#' \code{varnames} and \code{units} are assigned to returned vector with fixed
#' strings \code{"timestamp"} and \code{"-"}, respectively.
#'
#' Eddy covariance related measurements are usually stored with a timestamp
#' representing the end of the averaging period (typically 1800 s) in standard
#' time. This can however cause difficulties during data aggregation or
#' plotting. Therefore it is recommended to shift the date-time information
#' using \code{shift.by} to represent the center of averaging period prior to
#' any computations. It is also recommended to change the date-time information
#' to its original state before saving to a file (see Examples section).
#'
#' Any unsuccessful attempt to convert date-time information is considered to be
#' unexpected behavior and returns an error message instead of \code{NA} value.
#' In case that multiple formats are present in the timestamp, it has to be
#' corrected prior using \code{strptime_eddy}. It is expected that time series
#' are continuous even if no valid measurements are available for given time
#' interval. Therefore \code{interval} value is checked against the lagged
#' differences (\code{\link{diff}}) applied to the converted date-time vector
#' and returns an error message if mismatch is found. If \code{allow_gaps =
#' TRUE}, date-time information does not have to be regular but time differences
#' must be multiples of \code{interval}.
#'
#' The storage mode of returned POSIXct vector is forced to be integer instead
#' of double. This simplifies application of \code{\link{round_df}} but could
#' lead to unexpected behavior if the date-time information is expected to
#' resolve fractional seconds. Similarly \code{as.integer} is applied to
#' \code{shift.by} before it is added to the POSIXct vector to assure integer
#' storage mode of returned vector.
#'
#' @param x A character vector containing date-time information to be converted
#'   to class \code{"POSIXct"}.
#' @param format A character string. The default \code{format} is
#'   \code{"\%Y-\%m-\%d \%H:\%M"}
#' @param interval A numeric value specifying the time interval (in seconds) of
#'   the input date-time vector.
#' @param shift.by An integer value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If \code{TRUE}, date-time information does
#'   not have to be regular but time differences must be multiples of
#'   \code{interval}.
#' @param tz A time zone (see \code{\link{time zones}}) specification to be used
#'   for the conversion.
#' @param ... Further arguments to be passed from or to other methods.
#' @seealso \code{\link{strptime}} provides the details about conversions
#'   between date-time character representation and \code{"POSIXct"} or
#'   \code{"POSIXlt"} classes. It also includes information about \code{format}
#'   \emph{conversion specification}.
#'
#'   \code{\link{DateTimeClasses}} further inform about the date-time classes.
#'
#'   See \code{\link{locales}} to query or set a locale.
#' @examples
#' xx <- c("01.01.2014  00:30:00", "01.01.2014  01:00:00",
#' "01.01.2014  01:30:00", "01.01.2014  02:00:00")
#' varnames(xx) <- "timestamp"
#' units(xx) <- "-"
#' str(xx)
#' (yy <- strptime_eddy(xx, "%d.%m.%Y %H:%M", shift.by = -900L))
#' attributes(yy)
#' typeof(yy)
#'
#' ## Convert to original format
#' format(yy + 900, format = "%d.%m.%Y %H:%M", tz = "GMT")
#' zz <- xx[-3]
#' strptime_eddy(zz, "%d.%m.%Y %H:%M", allow_gaps = TRUE)
#'
#' \dontrun{
#' ## This is not a regular date-time sequence
#' strptime_eddy(zz, "%d.%m.%Y %H:%M") # error returned
#' ## interval argument provided incorrectly
#' strptime_eddy(xx, "%d.%m.%Y %H:%M", interval = 3600L)
#' }
#'
#' @export
strptime_eddy <- function(x, format = "%Y-%m-%d %H:%M", interval = 1800L,
                          shift.by = NULL, allow_gaps = FALSE, tz = "GMT",
                          ...) {
  if (anyNA(x)) stop("NAs in 'x' not allowed")
  out <- as.POSIXct(strptime(x, format = format, tz = tz, ...))
  storage.mode(out) <- "integer"
  if (anyNA(out)) stop("incorrect 'format' or multiple formats present")
  tdiff <- diff(as.integer(out))
  if (!allow_gaps && any(tdiff != interval)) {
    stop("timestamp does not form regular sequence with specified 'interval'")
  } else {
    # timestamp without gaps should have only one unique tdiff value
    if (length(unique(tdiff)) > 1) {
      message("timestamp in 'x' contains gaps")
      # gaps should be allowed only if they are multiples of interval
      if (any(tdiff %% interval != 0)) {
        stop("timestamp does not form regular sequence with 'interval' multiples")
      }
    }
  }
  if (!is.null(shift.by)) out <- out + as.integer(shift.by)
  varnames(out) <- "timestamp"
  units(out) <- "-"
  return(out)
}

#' Eddy Covariance Data Output
#'
#' Facilitates printing object \code{x} also with its units of measurement (or
#' space efficient metadata) to a file or \code{\link{connection}}.
#'
#' \code{write_eddy} extends the possibilities of \code{write.table} so the
#' units of measurement can also be written. However, it uses default arguments
#' of \code{write.csv} to provide flexibility for the user and to accomodate the
#' function for the most common case. The character string \code{"-9999"} is
#' typically used to represent missing values in eddy covariance (\emph{eddy})
#' data.
#'
#' Storing \code{varnames} and \code{units} attributes is practical mostly
#' within data frames and vectors. Attribute \code{varnames} extracted from each
#' data frame column represents names of respective variables and its main
#' purpose is to keep variable names of isolated vectors. Attribute \code{units}
#' extracted from each column represents units of measurement (or space
#' efficient metadata) of respective variables that are written one line above
#' data region. If the \code{varnames} or \code{units} attribute of given column
#' is \code{NULL}, of length not equal to 1, or contains missing value or empty
#' string, it is not considered meaningful. In that case the default column name
#' produced by \code{\link{as.data.frame}} is used instead (considered only if
#' \code{x} is supplied as vector) and unit of measurement is substituted with
#' \code{units_fill} string. \code{units_fill} can be an empty string.
#'
#' Units of measurement are considered to be part of the output header and
#' therefore \code{col.names} and \code{quote} arguments have the effect on the
#' way they are written.
#'
#' @param x The object that will be written. It is a data frame with optional
#'   attributes \code{units} and \code{varnames} assigned to each column.
#'   Otherwise it is converted by \code{\link{as.data.frame}}.
#' @param file Either a character string naming a file to write to or a
#'   \code{\link{connection}} that is open for writing. \code{""} results in
#'   writing to the console.
#' @param append A logical value. It is considered only if \code{file} is not a
#'   \code{connection}. If \code{TRUE}, the output is written below the content
#'   of the file. If \code{FALSE}, the content of the file is overwritten.
#' @param quote A logical value (\code{TRUE} or \code{FALSE}) or a numeric
#'   vector. If \code{TRUE}, columns of class character or factor will be
#'   surrounded by double quotes. If a numeric vector, its elements should mark
#'   the indices of character or factor columns to quote. In both cases, row and
#'   column names and units are quoted if present. If \code{FALSE}, no quoting
#'   is performed.
#' @param sep A character used as the field separator of each row.
#' @param units_fill A character string that represents missing value of
#'   \code{units} attribute in the output.
#' @param na A character string that represents missing data values in the
#'   output.
#' @param row.names Either a logical value (\code{TRUE} or \code{FALSE}) that
#'   determines if the row names of \code{x} should be included in the output,
#'   or a character vector of row names that will be used instead.
#' @param col.names Either a logical value (\code{TRUE}, \code{FALSE} or
#'   \code{NA}) or a character vector. If \code{TRUE}, column names of \code{x}
#'   will be included in the output. If a character vector, its elements will be
#'   used as column names. If \code{x} is supplied as vector, an attempt is made
#'   to extract meaningful variable name from its attribute \code{varnames}. In
#'   all cases, units extracted from \code{units} attribute of each column will
#'   be written one line below column names with identical format. See the 'CSV
#'   files' section in \code{\link{write.table}} for further explanation of
#'   \code{col.names = NA}.
#' @param qmethod A character string. It determines the way how strings quoting
#'   is performed in case of embedded double quote characters. The options are
#'   either \code{"double"} (\code{write.csv} and \code{write.csv2} defaults),
#'   that doubles the quote character, or \code{"escape"} (\code{write.table}
#'   default), that escapes it in C style by a backslash.
#' @param ... Further arguments to be passed to the internal
#'   \code{\link{write.table}} function.
#' @seealso \code{\link{write.table}} for information about full list of allowed
#'   arguments and their descriptions.
#'
#'   \code{\link{read_eddy}} to read data frame with \code{varnames} and
#'   \code{units} attributes specified for each column.
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
#' \dontrun{
#' # Example of using "col.names = NA"
#' zz <- file("ex.data", "w")  # open an output file connection
#' write_eddy(xx, zz, row.names = T, col.names = NA)
#' close(zz)
#' (ex_data <- read_eddy("ex.data", row.names = 1))
#' str(ex_data)
#' unlink("ex.data")}
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
#' \code{\link[openeddy]{openeddy}}.
#'
#' If \code{attr = "names"}, correction is meant to arrive to syntactically
#' valid names with a higher level of control. This assumes that the original
#' names were preserved during data loading (e.g. by using \code{check.names =
#' FALSE} in \code{\link{read_eddy}} or \code{\link{read.table}}). Specifically,
#' literal strings are renamed as: \itemize{\item \code{"(z-d)/L"} by
#' \code{"zeta"} \item \code{"qc_Tau"} by \code{"qc_Tau_SSITC"} \item
#' \code{"qc_H"} by \code{"qc_H_SSITC"} \item \code{"qc_LE"} by
#' \code{"qc_LE_SSITC"} \item \code{"qc_co2_flux"} by \code{"qc_NEE_SSITC"} }
#' and specified patterns or characters withing strings are substituted using
#' regular expression patterns: \itemize{\item \code{"co2_flux"} by \code{"NEE"}
#' \item \code{"*"} by \code{"star"} \item \code{"\%"} by \code{"perc"} \item
#' \code{"-"} and \code{"/"} by \code{"_"}.} After the substitutions
#' \code{make.names(names = x, ...)} is executed.
#'
#' If \code{attr = "units"}, round and square brackets are substituted by an
#' empty string.
#'
#' @param x A character vector.
#' @param attr A character string identifying an attribute type a character
#'   vector \code{x} to correct. Can be abbreviated.
#' @param ... Further arguments to be passed to the internal
#'   \code{\link{make.names}} function.
#'
#' @return A corrected character vector.
#'
#' @seealso \code{\link{make.names}}.
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
#' should \code{NA}s be interpreted.
#'
#' The quality checking results to combine must be provided as columns of a data
#' frame \code{x}, optionally with any number of further columns that will be
#' ignored. Columns specified by \code{qc_names} will be further separated
#' according to their additivity. For flags with fixed effect (\code{additive =
#' FALSE}; the most typical type), maximum is taken over each row. For flags
#' with additive effect (\code{additive = TRUE}), sum is taken over each row. In
#' case both types of flags are present, results for both groups are summed
#' together.
#'
#' The most typical value of argument \code{na.as} is \code{NA}. \code{NA} value
#' does not suggest any change in interpretation (value of variable
#' corresponding to this flag will be removed within quality checking scheme).
#' Exceptionally, value \code{0} can be used in case that the \code{NA} flag of
#' the quality checking test/filter is an expected result and means that the
#' half-hour was not checked by the given test/filter (e.g.
#' \code{\link{despikeLF}}).
#'
#' @section Automated recognition: Default values for \code{additive} and
#'   \code{na.as} arguments are \code{FALSE} and \code{NA}, respectively. In
#'   case that \code{additive_pattern} is found within \code{qc_names} (i.e.
#'   \code{qc_names} ending with \code{"interdep"} or \code{"wresid"} pattern),
#'   respective values of \code{additive} are changed to \code{TRUE}. This is
#'   because \code{\link{interdep}} and wresid (see \code{\link{extract_QC}})
#'   quality control checks are defined as additive within the current quality
#'   control scheme. If \code{na.as_0_pattern} is detected within
#'   \code{qc_names} (i.e. \code{qc_names} ending with \code{"spikesLF"},
#'   \code{"fetch70"} or \code{"man"} pattern), respective values of
#'   \code{na.as} are changed to \code{0} (see \code{\link{despikeLF}}).
#'
#' @return An integer vector with attributes \code{varnames} and \code{units} is
#'   produced. \code{varnames} value is set by \code{name_out} argument. Default
#'   value of \code{varnames} and \code{units} is set to \code{"-"}.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame \code{x} columns to combine.
#' @param name_out A character string providing \code{varnames} value of the
#'   output.
#' @param additive \code{NULL} or a vector of logical values (\code{TRUE} or
#'   \code{FALSE}) determining additivity of each respective column of \code{x}
#'   given by \code{qc_names}. If \code{NULL}, automated recognition is used.
#'   Otherwise, values determine if the flags should be treated as additive
#'   (\code{additive = TRUE}) or with fixed effect (\code{additive = FALSE}). If
#'   only one value is provided, all columns are considered to be of the same
#'   type.
#' @param na.as \code{NULL} or a vector of integer or \code{NA} values
#'   determining interpretation of missing flags in each respective column of
#'   \code{x} given by \code{qc_names}. If \code{NULL}, automated recognition is
#'   used. If only one value is provided, all columns are treated the same way.
#' @param additive_pattern A character string. A \code{\link[=regexp]{regular
#'   expression}} \code{\link{grep}} \code{pattern} identifying \code{qc_names}
#'   of flags with additive effect.
#' @param na.as_0_pattern A character string. A \code{\link[=regexp]{regular
#'   expression}} \code{\link{grep}} \code{pattern} identifying \code{qc_names}
#'   for which \code{NA} flags are interpreted as zeros.
#' @param no_messages A logical value.
#'
#' @seealso \code{\link{summary_QC}}.
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
#' Correction of matter or energy flux (\code{flux}) with storage computed using
#' discrete (one point) approach (\code{st}) or profile measurement of CO2
#' concentration (\code{stp}).
#'
#' If both storage estimates are available, \code{stp} takes priority. If both
#' \code{st} and \code{stp} estimates are \code{NA}, original flux value is
#' kept. \code{flux}, \code{st} and \code{stp} (if not NULL) must have the same
#' length.
#'
#' @return A vector with attributes \code{varnames} and \code{units} is
#'   produced. \code{varnames} value is set by \code{name_out} argument.
#'   \code{units} value is extracted from \code{flux} vector by
#'   \code{\link{units}} or set to default \code{"-"}.
#'
#' @param flux A numeric vector with flux values.
#' @param name_out A character string providing \code{varnames} value of the
#'   output.
#' @param st A numeric vector with storage computed using discrete
#'   (one point) approach.
#' @param stp A numeric vector with storage computed using
#' profile measurement of CO2.
#'
#' @seealso \code{\link{units}}.
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
#' offline in R (\code{REddyProc} package) or accessible online
#' (\href{http://www.bgc-jena.mpg.de/~MDIwork/eddyproc/upload.php}{Online Tool})
#' from the data frame \code{x}. Columns of data frame \code{x} ideally have
#' assigned attributes \code{varnames} and \code{units}.
#'
#' The typical variables (column names; i.e. \code{names_out}) required by the
#' tools (name; unit) are quality control of net ecosystem exchange
#' (\code{"qcNEE"}; \code{"-"}), net ecosystem exchange (\code{"NEE"};
#' \code{"umol m-2 s-1"}), quality control of latent heat (\code{"qcLE"};
#' \code{"-"}), latent heat (\code{"LE"}; \code{"W m-2"}), quality control of
#' sensible heat (\code{"qcH"}; \code{"-"}), sensible heat (\code{"H"}; \code{"W
#' m-2"}), global radiation (\code{"Rg"}; \code{"W m-2"}), air temperature
#' (\code{"Tair"}; \code{"degC"}), soil temperature (\code{"Tsoil"};
#' \code{"degC"}), relative humidity (\code{"rH"}; \code{"\%"}), vapor pressure
#' deficit(\code{"VPD"}; \code{"hPa"}), quality control of momentum flux
#' (\code{"qcTau"}; \code{"-"}) and friction velocity (\code{"Ustar"}; \code{"m
#' s-1"}). The unicode character for a greek letter micro (e.g. in NEE units) is
#' not accepted by the tools, thus it is substituted by simple \code{"u"}. Check
#' the gap-filling tool documentation for more details.
#'
#' \code{time_format} has two available options. \code{"YDH"} (default) extracts
#' columns Year, DoY (Day of year) and Hour (decimal number) from the timestamp
#' of \code{x}. It is less informative than \code{"YMDHM"} format but it is
#' supported by all versions of both offline and online tools. \code{"YMDHM"}
#' extracts columns Year, Month, Day, Hour, Minute. This format is not accepted
#' by the current
#' \href{https://www.bgc-jena.mpg.de/REddyProc/brew/REddyProc.rhtml}{Online
#' Tool}.
#'
#' Arguments \code{qcTau}, \code{qcH}, \code{qcLE} and \code{qcNEE} determine
#' whether the quality control will be applied to the respective fluxes. In case
#' of \code{qcTau}, quality control is applied to friction velocity (Ustar). If
#' \code{TRUE}, values of fluxes or friction velocity are set to \code{NA} when
#' respective quality control flag is \code{NA} or higher than \code{1}. In case
#' of \code{qcNEE}, NEE is also set to \code{NA} if respective values of Ustar
#' are \code{NA} (after application \code{qcTau} argument). This conservative
#' approach will assure that NEE values that cannot be compared against friction
#' velocity threshold (Ustar filtering) will be excluded.
#'
#' \code{check_time} checks the timestamp minutes that should be in format
#' specifying the end of measurement interval, i.e. [0, 30] instead of [15, 45].
#' Otherwise it produces warning. This check is designed only for data reported
#' in half-hourly intervals.
#'
#' \code{check_VPD} checks that the range of vapor pressure deficit (VPD) values
#' complies with assumption VPD <= 100. This is to check that VPD units are hPa,
#' not Pa.
#'
#' @param x A data frame with column names and \code{"timestamp"} column in
#'   POSIXt format.
#' @param names_in Column names (variables) present in \code{x} that will be
#'   used as input.
#' @param names_out Column names required by the tools for respective
#'   \code{names_in}.
#' @param time_format A character string identifying supported time format of
#'   the output. Can be abbreviated.
#' @param qcTau A logical value indicating whether quality control of momentum
#'   flux should be considered.
#' @param qcH A logical value indicating whether quality control of sensible
#'   heat flux should be considered.
#' @param qcLE A logical value indicating whether quality control of latent heat
#'   flux should be considered.
#' @param qcNEE A logical value indicating whether quality control of net
#'   ecosystem exchange should be considered.
#' @param check_time A logical value indicating whether timestamp should be
#'   checked.
#' @param check_VPD A logical value indicating whether range of vapor pressure
#'   deficit values should be checked.
#'
#' @seealso \code{\link{read_eddy}} and \code{\link{write_eddy}}.
#'
#' @encoding UTF-8
#' @export
set_OT_input <- function(x, names_in,
                         names_out = c("qcNEE", "NEE", "qcLE", "LE", "qcH",
                                       "H", "Rg", "Tair", "Tsoil", "rH", "VPD",
                                       "qcTau", "Ustar"),
                         time_format = c("YDH", "YMDHM"),
                         qcTau = TRUE, qcH = TRUE, qcLE = TRUE, qcNEE = TRUE,
                         check_time = TRUE, check_VPD = TRUE) {
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
  ts <- as.POSIXlt(x$timestamp)
  x <- x[names_in]
  units <- gsub("\u00B5", "u", units(x))
  for (i in seq_len(ncol(x))) {
    varnames(x[, i]) <- names_in[i]
    units(x[, i]) <- units[i]
  }
  names(x) <- names_out
  # Check works for half-hours but for hourly data both 9:00 and 9:30 format
  # will not throw warning (though 9:30 should)
  if (check_time && !all(ts$min %in% c(0, 30))) {
    warning("Timestamp minutes are not in required format [0, 30]",
            call. = FALSE)
  }
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
  if (check_VPD && "VPD" %in% names_out) {
    if (!all(is.na(out$VPD)) && any(out$VPD[!is.na(out$VPD)] > 100)) {
      warning("VPD input units are probably not in hPa", call. = FALSE)
    }
  }
  if (qcTau) {
    if (all(c("Ustar", "qcTau") %in% names_out)) {
      out$Ustar[out$qcTau > 1 | is.na(out$qcTau)] <- NA
    } else warning("qcTau skipped: missing 'Ustar' or 'qcTau'")
  }
  if (qcH) {
    if (all(c("H", "qcH") %in% names_out)) {
      out$H[out$qcH > 1 | is.na(out$qcH)] <- NA
    } else warning("qcH skipped: missing 'H' or 'qcH'")
  }
  if (qcLE) {
    if (all(c("LE", "qcLE") %in% names_out)) {
      out$LE[out$qcLE > 1 | is.na(out$qcLE)] <- NA
    } else warning("qcLE skipped: missing 'LE' or 'qcLE'")
  }
  if (qcNEE) {
    if (all(c("Ustar", "NEE", "qcNEE") %in% names_out)) {
      out$NEE[out$qcNEE > 1 | is.na(out$qcNEE) | is.na(out$Ustar)] <- NA
    } else warning("qcNEE skipped: missing 'Ustar', 'NEE' or 'qcNEE'")
  }
  return(out)
}

#' Quality Control Summary
#'
#' \code{summary_QC} is a function that summarizes quality checking results in a
#' form of table or plot.
#'
#' \code{summary_QC} loads a data frame \code{x}, extracts quality control (QC)
#' columns from it based on \code{qc_names} and creates a table (\code{plot =
#' FALSE}) or a plot (\code{plot = TRUE}) for these columns. Results are
#' displayed as percentages (\code{perc = TRUE}) or counts (\code{perc = FALSE})
#' for given flag and QC filter.
#'
#' \code{cumul = TRUE} specifies that cumulative effect of gradually applied QC
#' filters on resulting flags is considered. Note that for \code{cumul = TRUE}
#' the results do depend on the order of qc_names. \code{additive} is considered
#' only if \code{cumul = TRUE}, otherwise skipped.
#'
#' For a detailed description of automated recognition see
#' \code{\link{combn_QC}}.
#'
#' @return A table or a ggplot object depending on the \code{plot} argument
#'   value. If \code{length(qc_names) == 0}, \code{NULL} is returned instead.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame \code{x} columns to combine.
#' @param cumul A logical value that determines if cumulative (\code{cumul =
#'   TRUE}) or individual (\code{cumul = FALSE}) effects of quality control
#'   flags should be shown.
#' @param plot A logical value. If \code{TRUE}, the results are represented as a
#'   ggplot object. If \code{FALSE}, they are represented as a table.
#' @param perc A logical value. If \code{TRUE}, the results are reported in
#'   percentages. If \code{FALSE}, counts are used instead.
#' @param flux A character string. Used only if \code{plot = TRUE}. Includes the
#'   flux name in the plot title to emphasize the relevance of displayed quality
#'   control filters.
#' @param na.as \code{NULL} or a vector of integer or \code{NA} values
#'   determining interpretation of missing flags in each respective column of
#'   \code{x} given by \code{qc_names}. If \code{NULL}, automated recognition is
#'   used. If only one value is provided, all columns are treated the same way.
#' @param na.as_0_pattern A character string. A \code{\link[=regexp]{regular
#'   expression}} \code{\link{grep}} \code{pattern} identifying \code{qc_names}
#'   for which \code{NA} flags are interpreted as zeros.
#' @param additive \code{NULL} or a vector of logical values (\code{TRUE} or
#'   \code{FALSE}) determining additivity of each respective column of \code{x}
#'   given by \code{qc_names}. If \code{NULL}, automated recognition is used.
#'   Otherwise, values determine if the flags should be treated as additive
#'   (\code{additive = TRUE}) or with fixed effect (\code{additive = FALSE}). If
#'   only one value is provided, all columns are considered to be of the same
#'   type.
#' @param additive_pattern A character string. A \code{\link[=regexp]{regular
#'   expression}} \code{\link{grep}} \code{pattern} identifying \code{qc_names}
#'   of flags with additive effect.
#' @param no_messages A logical value.
#'
#' @seealso \code{\link{combn_QC}}, \code{\link{ggplot}}.
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
#' xx + ggplot2::theme(text = ggplot2::element_text(size = 20))
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
      ggplot2::aes(x = QC_filter, y = value, fill = QC_flag) +
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
#' of \code{\link{regular expression}} pattern matching multiple column names or
#' initializes one if missing.
#'
#' New data frame is created based on \code{x} and specified \code{source}.
#' Original \code{x} names are changed according to respective \code{new}
#' elements and kept as \code{varnames} attributes for tracebility. Accordingly,
#' if \code{qc} is specified, quality control columns are marked by \code{"qc_"}
#' prefix.
#'
#' If \code{regexp = FALSE} (the default), strictly one variable (column) will
#' be remapped to new name. The \code{source} elements must exactly match
#' \code{x} names, otherwise expected column is initialized with \code{NA}s. If
#' \code{qc} is specified, strictly one respective quality control column will
#' be renamed or skipped if not present.
#'
#' If \code{regexp = TRUE}, multiple columns can match the \code{source} element
#' \code{\link{regular expression}} pattern. In that case \code{\link{rowMeans}}
#' are produced and names of averaged columns kept as \code{varnames} attributes
#' for traceability. Similarly, also quality control flags are averaged over
#' available columns if \code{qc} is specified. Note that variable names need to
#' have unique patterns in order to achieve expected results. E.g. precipitation
#' abbreviated as P will have overlap with PAR; instead, sumP can be used.
#'
#' \code{varnames} attribute is expected. If not automatically assigned to
#' \code{x} through \code{\link{read_eddy}} when read from a file, they should
#' be assigned before remapping to keep documentation (especially if multiple
#' columns are combined to a single one).
#'
#' @param x data frame
#' @param new A character vector of new column names for remapping.
#' @param source A vector of \code{x} column names matching \code{new} to remap.
#'   If \code{regexp = TRUE}, character vector containing
#'   \code{\link[=regexp]{regular expression}}s.
#' @param regexp A logical value. If \code{FALSE} (the default), \code{source}
#'   will be interpreted literary. If \code{TRUE}, \code{source} elements will
#'   be used as \code{\link{grep}} \code{pattern}s.
#' @param qc A character string. A \code{\link{regular expression}}
#'   \code{\link{grep}} \code{pattern} identifying \code{x} column names that
#'   carry quality control information for respective \code{source}.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds. \code{na.rm} is used only if
#'   \code{regexp = TRUE} and multiple columns identified by \code{source} are
#'   combined by averaging.
#'
#' @return A data frame with attributes \code{varnames} and \code{units}
#'   assigned to each respective column.
#'
#' @seealso \code{\link{varnames}}.
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
      if (sum(!qc_index & index) >= 2) { # if multiple matches
        first <- which(!qc_index & index)[1]
        temp <- rowMeans(x[!qc_index & index], na.rm = na.rm)
        varnames(temp) <-
          paste0("mean(",
                 paste(varnames(x[!qc_index & index]), collapse = ", "),
                 ", na.rm = ", na.rm, ")")
        units(temp) <- units(x[first])
        out[i] <- temp
        if (!is.null(qc)) {
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
#' Merge generated regular date-time sequence with single or multiple data
#' frames.
#'
#' The primary purpose of \code{merge_eddy} is to combine chunks of data
#' vertically along their column \code{"timestamp"} with date-time information.
#' This \code{"timestamp"} is expected to be regular with given time
#' \code{interval}. Resulting data frame contains added rows with expected
#' date-time values that were missing in \code{"timestamp"} column, followed by
#' \code{NA}s. In case that \code{check_dupl = TRUE} and \code{"timestamp"}
#' values across \code{x} elements overlap, detected duplicated rows are removed
#' (the order in which duplicates are evaluated depends on the order of \code{x}
#' elements). A special case when \code{x} has only one element allows to fill
#' missing date-time values in \code{"timestamp"} column of given data frame.
#' Storage mode of \code{"timestamp"} column is set to be integer instead
#' of double. This simplifies application of \code{\link{round_df}} but could
#' lead to unexpected behavior if the date-time information is expected to
#' resolve fractional seconds.
#'
#' The list of data frames, each with column \code{"timestamp"}, is sequentially
#' \code{\link{merge}}d using \code{\link{Reduce}}. A \emph{(full) outer join},
#' i.e. \code{merge(..., all = TRUE)}, is performed to keep all columns of
#' \code{x} elements. The order of \code{x} elements can affect the result.
#' Duplicated column names within \code{x} elements are corrected using
#' \code{\link{make.unique}}. The merged data frame is then merged on the
#' validated \code{"timestamp"} column that can be either automatically
#' extracted from \code{x} or manually specified.
#'
#' For horizontal merging (adding columns instead of rows) \code{check_dupl =
#' FALSE} must be set but simple \code{\link{merge}} could be preferred.
#' Combination of vertical and horizontal merging should be avoided as it
#' depends on the order of \code{x} elements and can lead to row duplication.
#' Instead, data chunks from different data sources should be first separately
#' vertically merged and then merged horizontally in a following step.
#'
#' @param x List of data frames, each with \code{"timestamp"} column of class
#'   \code{"POSIXt"}. Optionally with attributes \code{varnames} and
#'   \code{units} for each column.
#' @param start,end A value specifying the first (last) value of the generated
#'   date-time sequence. If \code{NULL}, \code{\link{min}} (\code{\link{max}})
#'   is taken across the values in \code{"timestamp"} columns across \code{x}
#'   elements. If numeric, the value specifies the year for which the first
#'   (last) date-time value will be generated, considering given time
#'   \code{interval} and convention of assigning of measured records to the end
#'   of the time interval. Otherwise, character representation of specific half
#'   hour is expected with given \code{format} and \code{tz}.
#' @param check_dupl A logical value specifying whether rows with duplicated
#'   date-time values checked across \code{x} elements should be excluded before
#'   merging.
#' @param interval A numeric value specifying the time interval (in seconds) of
#'   the generated date-time sequence.
#' @param format A character string. Format of \code{start} (\code{end}) if
#'   provided as a character string.The default \code{\link[=strptime]{format}}
#'   is \code{"\%Y-\%m-\%d \%H:\%M"}.
#' @param tz A time zone (see \code{\link{time zones}}) specification to be used
#'   for the conversion of \code{start} (\code{end}) if provided as a character
#'   string.
#'
#' @return A data frame with attributes \code{varnames} and \code{units} for
#'   each column, containing date-time information in column \code{"timestamp"}.
#'
#' @seealso \code{\link{merge}}, \code{\link{Reduce}}, \code{\link{strptime}},
#'   \code{\link{time zones}}, \code{\link{make.unique}}
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
                       interval = NULL, format = "%Y-%m-%d %H:%M", tz = "GMT") {
  sq <- seq_len(length(x))
  check_x <- lapply(x, function(x) any(!is.data.frame(x),
                                       !inherits(x$timestamp, "POSIXt")))
  if (any(unlist(check_x)))
    stop(strwrap("'x' must be list of data frames with 'timestamp'
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
    # identify (row) position with duplicated timestamp across data frames
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
  if (is.null(interval)) {
    # automated estimation of interval
    # working on list is more reliable due to possible gaps among its elements
    interval <- median(do.call(c, lapply(x, function(x) diff(x$timestamp))))
    if (!length(interval)) {
      stop("not possible to automatically extract 'interval' from 'x'")
    } else {
      message("'interval' set to '", format(interval),
              "' - specify manually if incorrect")
    }
  } else {
    # convert 'interval' to class 'difftime'
    interval <- diff(seq(Sys.time(), by = interval, length.out = 2))
  }
  if (diff(range) < interval)
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

  # Force storage mode of timestamp to integer to simplify data frame rounding
  storage.mode(out$timestamp) <- "integer"

  return(out)
}

#' Read Meteorological Data with Units
#'
#' Read single or multiple CSV meteorological data files at Czechglobe MeteoDBS
#' format at given path and merge them together.
#'
#' This utility function is adapted to Czechglobe MeteoDBS file structure but
#' allows to change selected useful arguments that have preset default values.
#' It also assures that date-time sequence is regular and equidistant.
#'
#' In case that multiple files are present in the \code{path}, the expectation
#' is that files represent meteorological variables for given site and different
#' periods. Function merges them vertically (along generated complete
#' timestamp). All original columns across all files excluding the last empty
#' one are kept. The order of variables keeps that of the first file loaded
#' (note that file ordering in \code{path} is alphabetical not chronological)
#' and additional variables are appended if present in the following files. The
#' output "date/time" column is converted into class \code{POSIXct}.
#'
#' If you want to specify \code{start} and \code{end} arguments as strings and
#' you change also default \code{shift.by} value, \code{start} and \code{end}
#' arguments need to be adopted accordingly to account for that change. E.g. if
#' \code{shift.by = -900}, then \code{start = "2019-12-31 21:15:00", end =
#' "2019-12-31 23:15:00"} instead of \code{start = "2019-12-31 21:30:00", end =
#' "2019-12-31 23:30:00"}.
#'
#' Function introduces additional column "timestamp" for purposes of merging
#' with \code{merge_eddy()}. This column is then removed as it is not included
#' in the original data.
#'
#' @return A data frame is produced with additional attributes \code{varnames}
#'   and \code{units} assigned to each respective column.
#'
#' @param path A string. The path to directory with CSV file(s) in Czechglobe
#'   MeteoDBS format. Other than CSV files are ignored.
#' @param start,end A value specifying the first (last) value of the generated
#'   date-time sequence in temporary column "timestamp". If \code{NULL},
#'   \code{\link{min}} (\code{\link{max}}) of date-time values from "date/time"
#'   column across all files is used. If numeric, the value specifies the year
#'   for which the first (last) date-time value will be generated, considering
#'   given time interval (automatically detected from "date/time" column) and
#'   convention of assigning of measured records to the end of the time
#'   interval. Otherwise, character representation of specific date-time value
#'   is expected in given \code{format} and timezone "GMT".
#' @param format A character string. Format of \code{start} (\code{end}) if
#'   provided as a character string.
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If \code{TRUE}, date-time information does
#'   not have to be regular but time differences must be multiples of
#'   automatically detected time interval.
#' @param verbose A logical value. Should additional statistics about presence
#'   of \code{NA} values in resulting data frame be printed to console?
#'
#' @importFrom utils read.table
#' @export
read_MeteoDBS <- function(path, start = NULL, end = NULL,
                          format = "%d.%m.%Y %H:%M", shift.by = NULL,
                          allow_gaps = TRUE, verbose = TRUE) {
  lf <- list.files(path, full.names = TRUE)
  lf <- grep("\\.[Cc][Ss][Vv]$", lf, value = TRUE) # "\\." is literal dot
  if (length(lf) == 0) stop("no CSVs in folder specified by 'path'")
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
#' Read single or multiple CSV EddyPro full output files at given path and merge
#' them together.
#'
#' This utility function is adapted to EddyPro full output file structure but
#' allows to change selected useful arguments that have preset default values.
#' Column "timestamp" with date-time information is constructed based on "date"
#' and "time" columns and converted into class \code{POSIXct}. It also assures
#' that date-time sequence is regular and equidistant.
#'
#' In case that multiple files are present in the \code{path}, function merges
#' them vertically (along generated complete timestamp) and discards rows with
#' duplicated date-time values. All original columns across all files are kept.
#' The order of variables keeps that of the first file loaded (note that file
#' ordering in \code{path} is alphabetical not chronological) and additional
#' variables are appended if present in the following files. To assure
#' compatibility with older EddyPro versions, old column name "max_speed" is
#' renamed to "max_wind_speed" if present.
#'
#' If you want to specify \code{start} and \code{end} arguments as strings and
#' you change also default \code{shift.by} value, \code{start} and \code{end}
#' arguments need to be adopted accordingly to account for that change. E.g. if
#' \code{shift.by = -900}, then \code{start = "2019-12-31 21:15:00", end =
#' "2019-12-31 23:15:00"} instead of \code{start = "2019-12-31 21:30:00", end =
#' "2019-12-31 23:30:00"}.
#'
#' Note that \code{skip} and \code{fileEncoding} arguments must be valid across
#' all files, otherwise the function will not execute correctly.
#'
#' @return A data frame is produced with additional attributes \code{varnames}
#'   and \code{units} assigned to each respective column.
#'
#' @param path A string. The path to directory with EddyPro full output. Other
#'   than CSV files are ignored.
#' @param start,end A value specifying the first (last) value of the column
#'   "timestamp" in outputted data frame. If \code{NULL}, \code{\link{min}}
#'   (\code{\link{max}}) of date-time values from "timestamp" column across all
#'   input files is used. If numeric, the value specifies the year for which the
#'   first (last) date-time value will be generated, considering given time
#'   interval (automatically detected from "timestamp" column) and convention of
#'   assigning of measured records to the end of the time interval. Otherwise,
#'   character representation of specific date-time value is expected in given
#'   \code{format} and timezone "GMT".
#' @param skip An integer. The number of lines to skip in the input file before
#'   reading data.
#' @param fileEncoding A character string. If non-empty, declares the encoding
#'   used on a file (not a connection) so the character data can be re-encoded.
#'   See \code{\link{read.table}} for further details.
#' @param format A character string. Format of \code{start} (\code{end}) if
#'   provided as a character string.
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
#' @param allow_gaps A logical value. If \code{TRUE}, date-time information does
#'   not have to be regular but time differences must be multiples of
#'   automatically detected time interval.
#'
#' @export
read_EddyPro <- function(path, start = NULL, end = NULL, skip = 1,
                         fileEncoding = "UTF-8", format = "%Y-%m-%d %H:%M",
                         shift.by = NULL, allow_gaps = TRUE) {
  lf <- list.files(path, full.names = TRUE)
  lf <- grep("\\.[Cc][Ss][Vv]$", lf, value = TRUE) # "\\." is literal dot
  if (length(lf) == 0) stop("no CSVs in folder specified by 'path'")
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

#' Combine Documentation
#'
#' Read documentation from single or multiple TXT files. In case of multiple
#' files, combine them together with one additional line separating them.
#'
#' @param path A character vector. The full paths to TXT files.
#'
#' @seealso \code{\link{readLines}}.
#'
#' @export
combine_docu <- function(path) {
  unlist(lapply(path, function(x) c(readLines(x, warn = FALSE), "")))
}

#' Strip Positional Qualifier Suffix
#'
#' Function removes from variable name the suffix with three indices
#' representing horizontal and vertical placement and number of replicates
#' (_H_V_R suffix used in tower network naming strategy).
#'
#' If \code{warn = TRUE}, it is checked if multiple _H_V_R suffixes were
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
#'
#' @return A character vector with subset of expected variable names.
#'
#' @examples
#' all_names <- c("TA", "TS", "VPD", "LE", "H", "NEE")
#' names <- c("H", "LE", "PM10")
#' choose_avail(names, all_names)
#'
#' @export
choose_avail <- function(names, all_names, show_ignored = FALSE) {
  names <- na.omit(names)
  chosen <- names[names %in% all_names]
  not_avail <- setdiff(all_names, chosen)
  ignored <- setdiff(names, chosen)
  if (length(not_avail))
    message("Following names are not available:\n",
            paste(not_avail, collapse = ", "))
  if (show_ignored && length(ignored))
    message("Following names were ignored:\n",
            paste(ignored, collapse = ", "))
  return(chosen)
}

