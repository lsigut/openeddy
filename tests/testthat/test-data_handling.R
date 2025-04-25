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

# Test merge_eddy() - single data frame input ====

test_that("attributes are conserved for single data frame", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 1:5, 1:6)))
    ),
    # explicit check - following tests check it implicitly
    ex(eddy_data, 1:5, 1:6))
})

# validation data with descending timestamp and reset row names
ec <- ex(eddy_data, 5:1, 1:6)
row.names(ec) <- NULL

test_that("timestamp can be descending", {
  expect_equal(
    suppressMessages(merge_eddy(list(ex(eddy_data, 5:1, 1:6)))), ec)
})

test_that("x cannot be NULL value", {
  # catching exceptions for supporting NULL input seems not worth it
  # - users should handle NULL input beforehand
  expect_error(merge_eddy(list(NULL)), "list of data frames")
})

test_that("none of the chunks can be NULL value", {
  expect_error(merge_eddy(list(NULL, ex(eddy_data, 1:5))),
               "list of data frames")
})

test_that("single timestamp in x produces error without manual interval", {
  expect_error(
    merge_eddy(list(ex(eddy_data, 1, 1:6))),
    "not possible to automatically extract")
})

test_that("single timestamp in x produces error with manual interval", {
  expect_error(
    # there is no proper way of handling single timestamp (also no reason)
    merge_eddy(list(ex(eddy_data, 1, 1:6)), interval = 0),
    "wrong value of 'interval'")
})

test_that("timestamp can have gaps", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, c(1:2, 5:6), 1, drop = FALSE)))
    ),
    ex(eddy_data, 1:6, 1, drop = FALSE))
})

test_that("data frame can be shorter than produced timestamp - single value", {
  expect_equal(
    suppressMessages(
      # this might require to choose interval
      merge_eddy(list(ex(eddy_data, 3, 1, drop = FALSE)),
                 start = eddy_data[1, 1],
                 end = eddy_data[5, 1],
                 interval = 1800)
    ),
    ex(eddy_data, 1:5, 1, drop = FALSE))
})

test_that("data frame can be shorter than produced timestamp - more values", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 3:4, 1, drop = FALSE)),
                 start = eddy_data[1, 1],
                 end = eddy_data[5, 1])
    ),
    ex(eddy_data, 1:5, 1, drop = FALSE))
})

test_that("manually set interval can produce finer temporal resolution", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 3:4, 1, drop = FALSE)),
                 start = eddy_data[1, 1],
                 end = eddy_data[5, 1],
                 interval = 900)
    ),
    set_attributes(data.frame(timestamp = seq(eddy_data[1, 1],
                                              eddy_data[5, 1],
                                              by = "15 mins")),
                   "timestamp"))
})

test_that("data frame can be longer than produced timestamp", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 1:5, 1, drop = FALSE)),
                 start = eddy_data[3, 1],
                 end = eddy_data[4, 1])
    ),
    # drop dimensions to get rid of row.names (they would be 3 & 4)
    data.frame(timestamp = ex(eddy_data, 3:4, 1)))
})

# Test merge_eddy() - multiple data frames input ====

test_that("unordered chunks with aligned timestamp merge well", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 6:10, 1:6), ex(eddy_data, 1:5, 1:6)))
    ),
    ex(eddy_data, 1:10, 1:6))
})

test_that("multiple chunks with gaps merge properly", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 1:3, 1, drop = FALSE),
                      ex(eddy_data, 6:10, 1, drop = FALSE),
                      ex(eddy_data, 13, 1, drop = FALSE)))
    ),
    ex(eddy_data, 1:13, 1, drop = FALSE))
})

# test data demonstrating shifted timestamp (chunks not aligned)
ec <- ex(eddy_data, 4:6, 1, drop = FALSE)
ec$timestamp <- ec$timestamp + 900

test_that("misaligned timestamps take correct interval, range and values", {
  expect_equal(
    suppressMessages(
      # merge() sorts according to timestamp so result is not order-dependent
      merge_eddy(list(ec, ex(eddy_data, 1:3, 1, drop = FALSE)))
    ),
    set_attributes(
      data.frame(timestamp = seq(eddy_data[1, 1], ec[3, 1], by = "30 mins")),
      "timestamp")
    )
})

# test data demonstrating two-hourly data
ec <- ex(eddy_data, c(TRUE, FALSE, FALSE, FALSE), 1:6, drop = FALSE)
rownames(ec) <- NULL

test_that("two-hourly chunks with aligned timestamp merge well", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(ec, 6:10, 1:6), ex(ec, 1:5, 1:6)))
    ),
    ex(ec, 1:10, 1:6))
})

test_that("hourly and half-hourly chunks merge correctly", {
  expect_equal(
    suppressMessages(
      # --- varnames dropped (because only one column merged it seems - works for 2 cols)
      # data columns from both chunks will be merged
      merge_eddy(list(ex(eddy_data, c(1, 3), 1, drop = FALSE),
                      ex(eddy_data, 4:6, 1, drop = FALSE)))
    ),
    ex(eddy_data, 1:6, 1, drop = FALSE)
  )
})

test_that("chunk overlaps are handled correctly", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 6:10, 1:6), ex(eddy_data, 1:7, 1:6)))
    ),
    ex(eddy_data, 1:10, 1:6))
})

# reference data showing expected output
ec <- ex(eddy_data, 1:6, 1:6)
ec[1:3, 5:6] <- NA

test_that("chunks with different width merge well", {
  expect_equal(
    suppressMessages(
      # this implicitly tests also correct handling of attributes
      merge_eddy(list(ex(eddy_data, 1:3, 1:4), ex(eddy_data, 4:6, 1:6)))
    ),
    ec)
})

# reference data showing expected output
ec <- ex(eddy_data, 1:10, 1:6)
ec[6:10, 5:6] <- NA

test_that("results depend on element order for overlaps with different width", {
  expect_equal(
    suppressMessages(
      # see merge_eddy(list(ex(eddy_data, 1:7, 1:6), ex(eddy_data, 6:10, 1:4)))
      # see that check_dupl = FALSE works optimally when chunk values originate
      # from the same source data (results not depending on order)
      merge_eddy(list(ex(eddy_data, 6:10, 1:4), ex(eddy_data, 1:7, 1:6)))
    ),
    ec)
})

# Test merge_eddy() - check_dupl = FALSE & multiple data frames input ====

test_that("results do not depend on element order if check_dupl = FALSE", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 6:10, 1:4), ex(eddy_data, 1:7, 1:6)),
                 check_dupl = FALSE)
    ),
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 1:7, 1:6), ex(eddy_data, 6:10, 1:4)),
               check_dupl = FALSE)))
})

test_that("no timestamp duplication when chunk values match", {
  expect_equal(
    suppressMessages(
      merge_eddy(list(ex(eddy_data, 1:6, 1:6), ex(eddy_data, 1:6, 1:6)),
                 check_dupl = FALSE)
    ),
    ex(eddy_data, 1:6, 1:6))
})

# test data: ec1 - orig, ec2 - all cols match except one, ec3 - all cols NAs
# except 3rd (not matching ec1)
ec1 <- ec2 <- ec3 <- ex(eddy_data, 1:6, 1:6)
ec2[1:6, 2] <- 1:6
ec3[1:6, 2:6] <- NA
ec3[1:6, 3] <- letters[1:6]

# validation data - column type change due to the merge of char with numeric
ec4 <- ex(eddy_data, 1:6, 1:6)
ec4$PAR <- set_attributes(as.character(ec4$PAR), "PAR", "umol*m-2*s-1")

test_that("timestamp duplications are removed when data across chunks differ", {
  expect_equal(
    suppressWarnings(
      suppressMessages(
        # handle not matching data: remove timestamp duplicates
        # it seems hard to avoid type conversions
        merge_eddy(list(ec1, ec2, ec3), check_dupl = FALSE)
      )),
    ec4)
})

test_that("timestamp duplications produce correct warning", {
  expect_warning(
      suppressMessages(
        # handle not matching data: remove timestamp duplicates
        # it seems hard to avoid type conversions
        merge_eddy(list(ec1, ec2, ec3), check_dupl = FALSE)
      ),
    "overlapping parts of data frames do not have identical data")
})

# test data with all data columns as NAs
ec <- ex(eddy_data, 1:6, 1:6)
ec[1:6, 2:6] <- NA

test_that("timestamp duplications caused by cols with NAs are removed ", {
  expect_equal(
    suppressWarnings(
      suppressMessages(
      # NA is also a value (and if not matching leading to timestamp duplicates)
      # more complete rows (less NAs) seem to get higher line independent of
      # element order
      merge_eddy(list(ec, ex(eddy_data, 1:6, 1:6)), check_dupl = FALSE)
    )),
    ex(eddy_data, 1:6, 1:6))
})


# td <- tempdir()
# tf <- tempfile("testZZ", td, fileext = ".csv")
#
# write.table(t1, tf, sep = ",", row.names = FALSE)
# read.table(tf, T, sep = ",")
# read_EddyPro(td, pattern = "testZZ")
#
# trace(read_EddyPro, browser)
#
# t1 <- data.frame(EddyPro = c("date", "-", "2024-01-01"),
#                  Notes = c("time", "-", "12:00:00"))

