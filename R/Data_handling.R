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
#' xx <- matrix(1:20, nrow = 4)
#' xx[2, ] <- NA
#' allNA(xx, 2) # All columns have at least one non-missing value
#' allNA(xx, 1) # Second row has all values missing
#' \dontrun{
#' apply(xx, 1, max, na.rm = TRUE)
#' ## returns c(17, -Inf, 19, 20) and a warning message}
#' ## Skip the allNA row in apply()
#' apply(xx[!allNA(xx, 1), ], 1, max, na.rm = TRUE)
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
#' Attribute \code{varnames} contains variable name of respective column that is
#' identical with its column name. The main purpose of \code{varnames} attribute
#' is to keep variable name of a vector when it is separated from the original
#' data frame.
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
#' ## header = FALSE and units = FALSE:
#' zz <- read_eddy(header = FALSE, units = FALSE, text =
#' "24.1.2015,1.70
#' 24.1.2016,1.72")
#' str(zz)
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
  if (header && units) colnames(data) <- colnames(var_units)
  if (units) var_units[var_units %in% c("", NA)] <- units_fill
  for (i in seq_len(ncol(data))) {
    varnames(data[, i]) <- colnames(data)[i]
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
#' regular date-time sequence with given frequency. Additional attributes
#' \code{varnames} and \code{units} are assigned to returned vector with fixed
#' strings \code{"timestamp"} and \code{"-"}, respectively.
#'
#' Eddy covariance related measurements are usually stored with a timestamp
#' representing the end of the averaging period (typicaly 1800 s) in standard
#' time. This can however cause difficulties during data aggregation or
#' plotting. Therefore it is recommended to shift the date-time information
#' using \code{shift.by} to represent the center of averaging period prior to
#' any computations. It is also recommended to change the date-time information
#' to its original state before saving to a file (see Examples section).
#'
#' Any unsuccessful attempt to convert date-time information is considered to be
#' unexpected behaviour and returns an error message instead of \code{NA} value.
#' In case that multiple formats are present in the timestamp, it has to be
#' corrected prior using \code{strptime_eddy}. It is expected that time series
#' are continuous although no valid measurements are available for given time
#' interval. Therefore \code{freq} value is checked against the lagged
#' differences (\code{\link{diff}}) applied to the converted date-time vector
#' and returns an error message if mismatch is found.
#' @param x A character vector containing date-time information to be converted
#'   to class \code{"POSIXct"}.
#' @param format A character string. The default \code{format} is
#'   \code{"\%Y-\%m-\%d \%H:\%M"}
#' @param freq A numeric value specifying the frequency (in seconds) of the
#'   input date-time vector.
#' @param shift.by A numeric value specifying the time shift (in seconds) to be
#'   applied to the date-time information.
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
#' (yy <- strptime_eddy(xx, "%d.%m.%Y %H:%M", shift.by = -900))
#' ## Convert to original format
#' format(yy + 900, format = "%d.%m.%Y %H:%M", tz = "GMT")
#' attributes(yy)
#' \dontrun{
#' zz <- xx[-3]
#' ## This is not a regular date-time sequence
#' strptime_eddy(zz, "%d.%m.%Y %H:%M")
#' ## freq argument provided incorrectly
#' strptime_eddy(xx, "%d.%m.%Y %H:%M", freq = 3600)}
strptime_eddy <- function(x, format = "%Y-%m-%d %H:%M", freq = 1800,
                          shift.by = NULL, tz = "GMT", ...) {
  out <- as.POSIXct(strptime(x, format = format, tz = tz, ...))
  if (!length(out)) stop("'x' not supplied correctly")
  if (any(is.na(out))) stop("'x' and/or 'format' not supplied correctly")
  if (any(diff(as.numeric(out)) != freq)) {
    stop("timestamp does not form regular sequence with specified 'freq'")
  }
  if (!is.null(shift.by)) out <- out + shift.by
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
    header <- as.data.frame(units, stringsAsFactors = FALSE)
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

#' Combine Quality Checking Results
#'
#' Combine quality checking results depending whether they have a fixed or
#' cumulative effect or any combination of these effects. It is also checked how
#' should \code{NA}s be interpreted.
#'
#' The quality checking results must be provided as a data frame with columns
#' containing quality flags resulting from individual tests/filters. For all
#' flags over all rows, the maximum and cumulative sum is taken for columns with
#' fixed and additive effect, respectively.
#'
#' Typical values of argument \code{na.as} are \code{NA}, \code{0} or \code{2}.
#' \code{NA} value is only formal and does not suggest any change in
#' interpretation. Value \code{0} is used in the case that the \code{NA} value
#' of the quality test/filter is an expected result and means that the half-hour
#' was not checked by or has a good quality according to the given test/filter
#' (e.g. \code{\link{despikeLF}}). Value \code{2} would be used only if the user
#' wants to explicitly specify that \code{NA} flag value should not be used for
#' further analyses.
#'
#' @return A vector with attributes \code{varnames} and \code{units} is
#'   produced. \code{varnames} value is set by \code{name_out} argument and
#'   \code{units} value is set to default \code{"-"}.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame columns to combine.
#' @param name_out A character string providing \code{varnames} value of the
#'   output.
#' @param additive A vector of logical values (\code{TRUE} or \code{FALSE}) for
#'   the \code{qc_names} subset of \code{x} that determines if the flags should
#'   be treated as additive (\code{additive = TRUE}) or with fixed effect
#'   (\code{additive = FALSE}). If only one value is provided, all columns are
#'   considered to be of the same type.
#' @param na.as A vector of numeric or \code{NA} values for the \code{qc_names}
#'   subset of \code{x} that determines how should be the missing flags
#'   interpreted. If only one value is provided, all columns are treated the
#'   same way.
#'
#' @seealso \code{\link{summary_QC}}.
#'
#' @examples
#' set.seed(5)
#' aa <- data.frame(xx = sample(c(0:2, NA), 20, replace = T))
#' aa$yy <- sample(c(0:2, NA), 20, replace = T)
#' aa$add_F <- combn_QC(aa, qc_names = c("xx", "yy"), additive = F,
#' name_out = "add_F")
#' aa$add_T <- combn_QC(aa, qc_names = c("xx", "yy"), additive = T,
#' name_out = "add_T")
#' aa$add_F_na.as_0 <- combn_QC(aa, qc_names = c("xx", "yy"), additive = F,
#' na.as = 0, name_out = "add_F_na.as_0")
#' aa$add_F_na.as_0part <- combn_QC(aa, qc_names = c("xx", "yy"), additive = F,
#' na.as = c(0, NA), name_out = "add_F_na.as_0part")
#' aa$add_F_na.as_2 <- combn_QC(aa, qc_names = c("xx", "yy"), additive = F,
#' na.as = 2, name_out = "add_F_na.as_2")
combn_QC <- function(x, qc_names, name_out, additive = FALSE, na.as = NA) {
  x_names <- colnames(x)
  name_out <- name_out[1]
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!is.character(qc_names)) stop("'qc_names' must be of class character")
  if (!is.logical(additive) || anyNA(additive) || length(additive) == 0) {
    stop("'additive' must be logical vector with non-missing values")
  }
  if (length(na.as) == 0 || (!is.numeric(na.as) && !all(is.na(na.as)))) {
    stop("'na.as' must be a vector containing numeric or NA values")
  }
  if (!is.character(name_out)) stop("'name_out' must be of class character")
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
  if (!all(qc_names %in% x_names)) {
    stop(paste("missing", paste0(qc_names[!(qc_names %in% x_names)],
                                 collapse = ", ")))
  }
  df <- x[c(qc_names)]
  if (length(qc_names) == 0) {
    return(df)
  }
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
#' aa <- matrix(ncol = 3, nrow = 10, byrow = T, c(-1, 1, 2),
#'              dimnames = list(NULL, c("flux", "st", "stp")))
#' aa[c(4, 8, 9, 11, 15, 18, 22, 25, 27, 29)] <- NA
#' (aa <- as.data.frame(aa))
#' aa$flux_stc <- add_st(aa$flux, "flux_stc", aa$st, aa$stp)
#' lapply(aa, attributes)
add_st <- function(flux, name_out, st, stp = NULL) {
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
#' @encoding UTF-8
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
#' \code{summary_QC} loads a data frame \code{x}, extracts quality control
#' columns from it based on \code{qc_names} and creates a table (\code{plot =
#' FALSE}) or a plot (\code{plot = TRUE}) for these columns. Results are
#' displayed in percentage (\code{perc = TRUE}) or counts (\code{perc = FALSE})
#' of given flag per dataset.
#'
#' \code{cumul = TRUE} specifies that cumulative effect of flags is considered
#' for each halfhour. Note that for \code{cumul = TRUE} the results do depend on
#' the order of qc_names. The flags on each row are cumulatively summed from
#' left to right. \code{additive} is used only if cumul = TRUE, otherwise
#' skipped.
#'
#' @return A table or a plot depending on the \code{plot} argument value.
#'
#' @param x A data frame with column names.
#' @param qc_names A vector of names of data frame columns to combine.
#' @param na.as A vector of numeric or \code{NA} values for the \code{qc_names}
#'   subset of \code{x} that determines how should be the missing flags
#'   interpreted. If only one value is provided, all columns are treated the
#'   same way.
#' @param cumul A logical value that determines if cumulative (\code{cumul =
#'   TRUE}) or individual (\code{cumul = FALSE}) effects of quality control
#'   flags should be shown.
#' @param additive A vector of logical values (\code{TRUE} or \code{FALSE}) for
#'   the \code{qc_names} subset of \code{x} that determines if the flags should
#'   be treated as additive (\code{additive = TRUE}) or with fixed effect
#'   (\code{additive = FALSE}). If only one value is provided, all columns are
#'   considered to be of the same type.
#' @param plot A logical value. If \code{TRUE}, the results are plotted in the
#'   active graphical device. If \code{FALSE}, they are represented as a table.
#' @param perc A logical value. If \code{TRUE}, the results are reported in
#'   percentages. If \code{FALSE}, counts are used instead.
#' @param flux A character string. Used only if \code{plot = TRUE}. Includes the
#'   flux name in the plot title to emphasise the relevance of displayed test
#'   types.
#'
#' @seealso \code{\link{combn_QC}}.
#'
#' @examples
#' set.seed(5)
#' aa <- as.data.frame(replicate(
#' 4, sample(c(0:2, NA), 20, replace = T, prob = c(rep(0.3, 3), 0.1))))
#' names(aa) <- letters[1:4]
#'
#' bb <- summary_QC(aa, letters[1:4])
#' summary_QC(aa, letters[1:4], na.as = c(NA, 0, NA, 2))
#' summary_QC(aa, letters[1:4], cumul = T, additive = T)
#' summary_QC(aa, letters[1:4], cumul = T, additive = F)
#' summary_QC(aa, letters[1:4], cumul = T, additive = c(F, T, F, T))
#' cc <- summary_QC(aa, letters[1:4], cumul = F, additive = c(F, T, F, T))
#' identical(bb, cc) # Argument additive is skipped when cumul = FALSE
#'
#' library(ggplot2)
#' (xx <- summary_QC(aa, letters[1:4], cumul = T, additive = T, plot = T,
#' flux = "CO2 flux"))
#' xx + ggplot2::theme(text = ggplot2::element_text(size = 20))
#'
#' summary_QC(aa, rep(letters[1:4], 2), cumul = T, additive = T, perc = F)
#' summary_QC(aa, rep(letters[1:4], 2), cumul = T, additive = T, plot = T,
#'            perc = F)
summary_QC <- function(x, qc_names, na.as = NA, cumul = FALSE, additive = FALSE,
                       plot = FALSE, perc = TRUE, flux = NULL) {
  x_names <- colnames(x)
  if (!is.data.frame(x) || is.null(x_names)) {
    stop("'x' must be of class data.frame with colnames")
  }
  if (!is.character(qc_names)) stop("'qc_names' must be of class character")
  if (length(na.as) == 0 || (!is.numeric(na.as) && !all(is.na(na.as)))) {
    stop("'na.as' must be a vector containing numeric or NA values")
  }
  if (length(na.as) > 1) {
    if (length(qc_names) != length(na.as)) {
      stop("'na.as' must be of same lenght as 'qc_names' or length 1")
    }
  } else if (length(na.as) != length(qc_names)) {
    na.as <- rep(na.as, length(qc_names))
  }
  if (!all(qc_names %in% x_names)) {
    stop(paste("missing", paste0(qc_names[!(qc_names %in% x_names)],
                                 collapse = ", ")))
  }
  df <- x[1:nrow(x), qc_names]
  if (!all(is.na(na.as))) {
    for (i in seq_along(qc_names)) {
      df[is.na(df[i]), i] <- na.as[i]
    }
  }
  if (cumul) {
    if (!is.logical(additive) || anyNA(additive) || length(additive) == 0) {
      stop("'additive' must be logical vector with non-missing values")
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
  }
  df_m <- reshape2::melt(df, id.vars = NULL, variable.name = "QC_type",
                        value.name = "QC_flag")
  if (perc) {
    tab <- round(table(df_m, useNA = "ifany") / (nrow(df) / 100), 1)
  } else {
    tab <- table(df_m, useNA = "ifany")
  }
  if (!plot) {
    return(tab)
  } else {
    tab_m <- reshape2::melt(tab, variable.name = "QC_type")
    if (cumul) {
      high5 <- tab_m$QC_flag > 5
      high5[is.na(high5)] <- FALSE
      tab_m[high5, "QC_flag"] <- "6+"
    }
    tab_m$QC_flag <- as.factor(paste("Flag", tab_m$QC_flag, sep = "_"))
    title <- ifelse(cumul,
                    paste0(c(flux, "Cumulative effect of QC flags"),
                           collapse = " - "),
                    paste0(c(flux, "Independent QC flags"),
                           collapse = " - "))
    y_label <- ifelse(perc, "Percentage", "Count")
    ggplot2::ggplot(tab_m) +
      ggplot2::aes(x = QC_type, y = value, fill = QC_flag) +
      ggplot2::geom_bar(stat = 'identity',
                        position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::scale_fill_hue(guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::labs(title = title, y = y_label) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 20, vjust = 1, hjust = 1))
  }
}
