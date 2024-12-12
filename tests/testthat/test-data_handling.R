# Function to setup varnames and units in expected output ====
set_attributes <- function(x, varnames = "test", units = "-") {
  varnames(x) <- varnames
  units(x) <- units
  return(x)
}

# Test infer_interval() ====
test_that("shortest interval extent is selected", {
  expect_equal(openeddy:::infer_interval(c(1:5, 2)),
               1)
})

test_that("the shortest interval is selected for descending intervals", {
  expect_equal(openeddy:::infer_interval(c(-2, -1, -1, -2)),
               -1)
})

test_that("interval can be double", {
  expect_equal(openeddy:::infer_interval(rep(1.15, 2)),
               1.15)
})

test_that("NULL is accepted", {
  expect_equal(openeddy:::infer_interval(NULL),
               NULL)
})

test_that("NA is accepted", {
  expect_equal(openeddy:::infer_interval(NA),
               NA_real_)
})

test_that("zero length vector produces NULL", {
  expect_equal(openeddy:::infer_interval(numeric(0)),
               NULL)
})

test_that("intervals are non-zero (timestamp representing a sequence)", {
  expect_error(openeddy:::infer_interval(-1:1),
               "date-time sequence includes duplicated timestamp")
})

test_that("intervals cannot be both positive and negative", {
  expect_error(openeddy:::infer_interval(c(-3:-1, 1:3)),
               "date-time sequence is both ascending and descending")
})

# Test strptime_eddy() ====
test_that("NULL value is accepted", {
  expect_equal(strptime_eddy(NULL),
               set_attributes(as.POSIXct(NULL, tz = "GMT"), "timestamp"))
})

test_that("single value is accepted", {
  expect_equal(strptime_eddy("2024-01-01 12:00"),
               set_attributes(ISOdate(2024, 1, 1), "timestamp"))
})

test_that("default type of timestamp is integer", {
  expect_equal(typeof(strptime_eddy("2024-01-01 12:00")),
               "integer")
})

test_that("timestamp typeof() stays integer after shift", {
  expect_equal(typeof(strptime_eddy("2024-01-01 12:00",
                                    storage.mode = "integer", shift.by = -900)),
               "integer")
})

test_that("timestamp typeof() can be changed to double", {
  expect_equal(typeof(strptime_eddy("2024-01-01 12:00",
                                    storage.mode = "double")),
               "double")
})

test_that("NA in x leads to error", {
  expect_error(strptime_eddy(c("2024-01-01 12:00", NA)),
               "NAs in 'x' not allowed")
})

test_that("interval manually set to 0 produces error", {
  expect_error(strptime_eddy(c("2024-01-01 12:00", "2024-01-01 13:00"),
                             interval = 0),
               "wrong value of 'interval'")
})

test_that("interval manually set to NA produces error", {
  expect_error(strptime_eddy(c("2024-01-01 12:00", "2024-01-01 13:00"),
                             interval = NA),
               "wrong value of 'interval'")
})

test_that("incorrect format leads to error", {
  expect_error(strptime_eddy(c("2024-01-01 12:00", "2024-01-01")),
               "incorrect 'format' or multiple formats present")
})

# case with implicit gap (based on user input instead of data)
test_that("if allow_gaps = FALSE, lower manual interval means gap is present", {
  expect_error(strptime_eddy(c("2024-01-01 12:00", "2024-01-01 13:00"),
                             interval = 1800),
               "timestamp in 'x' contains gaps")
})

test_that("if allow_gaps = FALSE, higher manual interval means wrong value", {
  expect_error(
    strptime_eddy(c("2024-01-01 12:00", "2024-01-01 13:00"), interval = 7200),
    "intervals among timestamps are not multiples of set 'interval'")
})

test_that("if allow_gaps = TRUE, error if gaps are not interval multiples", {
  expect_error(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 13:25"),
    allow_gaps = TRUE),
    "intervals among timestamps are not multiples of set 'interval'")
})

# case with explicit gap (based on data instead of user input)
test_that("if allow_gaps = FALSE, gap is automatically detected", {
  expect_error(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 15:00")),
    "timestamp in 'x' contains gaps")
})

test_that("if allow_gaps = TRUE, both implicit and explicit gaps work", {
  expect_equal(suppressMessages(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 15:00"),
    allow_gaps = TRUE, interval = 1800)),
    set_attributes(c(ISOdate(2024, 1, 1, 12), ISOdate(2024, 1, 1, 13),
                     ISOdate(2024, 1, 1, 15)),
                   "timestamp"))
})

test_that("if allow_gaps = TRUE, automatic interval inference works", {
  expect_equal(suppressMessages(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 15:00"),
    allow_gaps = TRUE)),
    set_attributes(c(ISOdate(2024, 1, 1, 12), ISOdate(2024, 1, 1, 13),
                     ISOdate(2024, 1, 1, 15)),
                   "timestamp"))
})

test_that("duplicated timestamps will lead to an error", {
  expect_error(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 12:00", "2024-01-01 15:00")),
    "date-time sequence includes duplicated timestamp")
})

test_that("descending x works with correct interval", {
  expect_equal(strptime_eddy(c("2024-01-01 12:00", "2024-01-01 11:00"),
                             interval = -3600),
               set_attributes(c(ISOdate(2024, 1, 1), ISOdate(2024, 1, 1, 11)),
                              "timestamp"))
})

test_that("timestamp cannot be both ascending and descending - auto int", {
  expect_error(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 12:00")),
    "date-time sequence is both ascending and descending")
})

test_that("timestamp cannot be both ascending and descending - manual int", {
  expect_error(strptime_eddy(
    c("2024-01-01 12:00", "2024-01-01 13:00", "2024-01-01 12:00"),
    interval = 3600, allow_gaps = TRUE),
    "date-time sequence is both ascending and descending")
})

# Test merge_eddy() ====
# - check that it does not fail with single value (simplify arg in sapply)
#   if (any(sapply(x, function(x) anyNA(x$timestamp))))
#      stop("'timestamp' includes NA value(s)")


# td <- tempdir()
# tf <- tempfile("testZZ", td, fileext = ".csv")
#
# write.table(t1, tf, sep = ",", row.names = FALSE)
# read.table(tf, T, sep = ",")
# read_EddyPro(td, pattern = "testZZ")
#
# trace(read_EddyPro, browser)
#
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
# t1 <- data.frame(EddyPro = c("date", "-", "2024-01-01"),
#                  Notes = c("time", "-", "12:00:00"))

