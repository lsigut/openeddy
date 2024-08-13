# NAs in x and thr lead to an error and do not have to be checked

# Function to setup varnames and units in expected output ====
set_attributes <- function(x, varnames = "test", units = "-") {
  varnames(x) <- varnames
  units(x) <- units
  return(x)
}

# Test apply_thr() ====
test_that("apply_thr Value is integer", {
  expect_equal(is.integer(apply_thr(1:9, c(3, 6), "test")), TRUE)
})

test_that("apply_thr flags higher correctly", {
  expect_equal(apply_thr(1:9, c(3, 6), "test"),
               set_attributes(rep(0:2, rep(3, 3))))
})

test_that("apply_thr flags outside correctly", {
  expect_equal(apply_thr(1:9, thr = c(4, 6), "test", "outside"),
               set_attributes(rep(c(2, 0, 2), rep(3, 3))))
})

test_that("apply_thr flags between correctly", {
  expect_equal(apply_thr(1:9, thr = c(3, 7), "test", "between"),
               set_attributes(rep(c(0, 2, 0), rep(3, 3))))
})

test_that("apply_thr flags lower correctly", {
  expect_equal(apply_thr(0:8, thr = c(6, 3), "test", "lower"),
               set_attributes(rep(2:0, rep(3, 3))))
})

test_that("both thr values identical work in apply_thr correctly", {
  expect_equal(apply_thr(c(2, 2.1), c(2, 2), "test"),
               set_attributes(c(0, 2)))
  expect_equal(apply_thr(c(2, 2.1), c(2, 2), "test", "outside"),
               set_attributes(c(0, 2)))
  expect_equal(apply_thr(c(2, 1.9), c(2, 2), "test", "lower"),
               set_attributes(c(0, 2)))
})

test_that("flag = 'higher' does not accept list as thr", {
  expect_error(apply_thr(1, list(c(0, 1), c(2, 3))),
               "'thr' supplied incorrectly")
})

test_that("flag = 'outside' does accept list as thr", {
  expect_equal(apply_thr(1, list(c(0, 1), c(2, 3)), "test", flag = "outside"),
               set_attributes(c(0)))
})

test_that("flag = 'outside' reports wrong thr", {
  expect_error(apply_thr(1, 1, flag = "outside"),
               "'thr' supplied incorrectly")
})

test_that("apply_thr accepts NULL", {
  expect_equal(apply_thr(NULL, c(3, 6), "test", flag = "higher"),
               set_attributes(integer(0)))
})

# Test flag_runs() ====
test_that("flag_runs Value is integer", {
  expect_equal(is.integer(flag_runs(c(1, rep(c(0, NA), 2)), "test")), TRUE)
})

test_that("flag_runs handles NAs", {
  expect_equal(flag_runs(c(1, 3, 3, NA, NA, 3, 3), "test"),
               set_attributes(c(0, 2, 2, NA, NA, 2, 2)))
})

test_that("flag_runs catches repeated values", {
  expect_equal(flag_runs(rep(1:6, rep(c(2, 1), 3)), "test", length = 2),
               set_attributes(rep(c(2, 2, 0), 3)))
})

# Test flag_periods() ====
ts <- seq(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"),
          by = "30 mins")
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 13, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 16, 15, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 14, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 17, 15, 0, tz = "GMT"))
)

test_that("periods are flagged correctly", {
  expect_equal(
    flag_periods(ts, periods$start, periods$end, "test"),
    set_attributes(rep(c(0, 2, 0, 2, 0), c(2, 3, 3, 3, 2))))
})

test_that("flags are integers", {
  expect_equal(
    class(flag_periods(ts, periods$start, periods$end, "test")),
    "integer")
})

# test for opposite order of start and end of periods (first case in periods)
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 15, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("opposite order of start and end shows error", {
  expect_error(
    flag_periods(ts, periods$start, periods$end, "test"),
    "'start' value higher than 'end' value")
})

# opposite order within start shows error
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 14, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("opposite order within start shows error", {
  expect_error(
    flag_periods(ts, periods$start, periods$end, "test"),
    "'start' and 'end' must be in ascending order")
})

# Test label_periods() ====

# test for correct class of output depending on the class of labels
ts <- seq(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"),
          by = "30 mins")
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 15, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("output labels have correct class (char/num/log/int)", {
  expect_equal(
    c(class(label_periods(ts, letters[1:2], periods$start, periods$end)),
      class(label_periods(ts, 1:2, periods$start, periods$end)),
      class(label_periods(ts, c(TRUE, FALSE), periods$start, periods$end)),
      class(label_periods(ts, c(1.1, 1.2), periods$start, periods$end))),
    c("character", "integer", "logical", "numeric"))
})

# test for overlap of periods
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 17, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("overlap of periods is handled correctly", {
  expect_equal(
    label_periods(ts, letters[1:2], periods$start, periods$end, "test"),
    set_attributes(rep(letters[1:2], c(7, 6))))
})

# test for periods starting/ending before/after ts
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 15, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 20, 15, 0, tz = "GMT"))
)

test_that("periods outside of timestamp are handled correctly", {
  expect_equal(
    label_periods(ts, letters[1:2], periods$start, periods$end, "test"),
    set_attributes(rep(letters[1:2], c(7, 6))))
})

# test for periods without labels
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 14, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("periods without labels are handled correctly", {
  expect_equal(
    label_periods(ts, letters[1:2], periods$start, periods$end, "test"),
    set_attributes(rep(c("a", NA, "b"), c(5, 2, 6))))
})

# test for opposite order of start and end of periods (first case in periods)
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 15, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("opposite order of start and end shows error", {
  expect_error(
    label_periods(ts, letters[1:2], periods$start, periods$end, "test"),
    "'start' value higher than 'end' value")
})

# opposite order within start shows error
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 14, 15, 0, tz = "GMT"),
          ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("opposite order within start shows error", {
  expect_error(
    label_periods(ts, letters[1:2], periods$start, periods$end, "test"),
    "'start' and 'end' must be in ascending order")
})

# test that testing of opposite order with diff() does not fail for single value
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 12, 15, 0, tz = "GMT")),
  end = c(ISOdatetime(2000, 1, 1, 18, 15, 0, tz = "GMT"))
)

test_that("opposite order within start - no error with start of length 1", {
  expect_equal(
    label_periods(ts, "a", periods$start, periods$end, "test"),
    set_attributes(rep("a", 13)))
})

# test for periods defined only by start (end = NULL)
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT"))
)

test_that("end can be NULL", {
  expect_equal(
    label_periods(ts, letters[1:2], periods$start, name_out = "test"),
    set_attributes(rep(letters[1:2], c(7, 6))))
})

test_that("end = NULL works also with single start value", {
  expect_equal(
    label_periods(ts, "a", periods$start[1], name_out = "test"),
    set_attributes(rep("a", 13)))
})

# test special case if end = NULL and start is set outside of x range
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 20, 45, 0, tz = "GMT"))
)

test_that("end = NULL works if start is outside of 'x' range", {
  expect_equal(
    label_periods(ts, letters[1:3], periods$start, name_out = "test"),
    set_attributes(rep(letters[1:2], c(7, 6))))
})

# test that end = NULL produces error if start is in descending order
periods <- data.frame(
  start = c(ISOdatetime(2000, 1, 1, 15, 45, 0, tz = "GMT"),
            ISOdatetime(2000, 1, 1, 10, 15, 0, tz = "GMT"))
)

test_that("end = NULL fails if start has descending order", {
  expect_error(
    label_periods(ts, letters[1:2], periods$start, name_out = "test"),
    "'start' must be in ascending order")
})

# Test separate() ====
test_that("test that output is list", {
  expect_equal(
    class(openeddy:::separate(x = 800000099, "[8]")),
    "list")
})

test_that("test proper char count", {
  expect_equal(
    length(openeddy:::separate(x = 800000099, "[8]")[[1]]),
    nchar("00000099"))
})

test_that("test that separation of units works", {
  expect_equal(
    openeddy:::separate(x = "8u/v/w/ts/co2/h2o/ch4/none", "[8]", split = "/"),
    list(c("u", "v", "w", "ts", "co2", "h2o", "ch4", "none")))
})

test_that("test that NA produces single value NA", {
  expect_equal(
    openeddy:::separate(x = NA, "[8]")[[1]],
    NA_character_)
})

# Test extract_coded() ====
test_that("x is expected to include units", {
  expect_error(
    extract_coded(0),
    "missing units containing the format of coded variable")
})

coded <- data.frame(attangle = 81,
                    nonsteady = 80,
                    timelag_hf = 80000,
                    abslim = 800000099)
units(coded) <- c("8aa",
                  "8U",
                  "8co2/h2o/ch4/none",
                  "8u/v/w/ts/co2/h2o/ch4/none")

test_that("support for attack angle works", {
  expect_equal(
    extract_coded(coded$attangle, name_out_ALL = "test"),
    set_attributes(data.frame(test = 2)))
})

test_that("support for nonsteady works", {
  expect_equal(
    extract_coded(coded$nonsteady, name_out_ALL = "test"),
    set_attributes(data.frame(test = 0)))
})

test_that("support for timelag works", {
  expect_equal(
    extract_coded(coded$timelag_hf, name_out_GA = "test"),
    set_attributes(data.frame(test = 0)))
})

test_that("support for abslim works", {
  expect_equal(
    extract_coded(coded$abslim, name_out_SA = "test1", name_out_SAGA = "test2"),
    set_attributes(data.frame(test1 = 0, test2 = 0),
                   c("test1", "test2"), c("-", "-")))
})

# Test mf() ===========
test_that("test that missing fraction works", {
  expect_equal(
    openeddy:::mf(data.frame(0, 0), c(36000 / c(1, 2, 5)), 36000),
    c(0, 0.5, 0.8))
})

test_that("test that used_records is not higher than max_records", {
  expect_error(
    openeddy:::mf(data.frame(0, 0), 36000, 18000),
    "used_records higher than max_records")
})

# Test extract_QC() - wresid filter ====
test_that("double rotation flagging works", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(w_unrot = c(0.001, 0.4)), filters = "wresid")
    ),
    set_attributes(data.frame(qc_ALL_wresid = c(0, 2)),
                   varnames = "qc_ALL_wresid"))
})

test_that("double rotation flagging is skipped", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(miss = 1:3), filters = "wresid")
    ),
    data.frame(1:3)[0])
})

test_that("planar fit rotation flagging works", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(w_rot = c(0.001, 0.11, 0.16)),
                 filters = "wresid", rotation = "planar fit")
    ),
    set_attributes(data.frame(qc_ALL_wresid = 0:2),
                   varnames = "qc_ALL_wresid"))
})

test_that("planar fit flagging is skipped", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(miss = 1:3),
                 filters = "wresid", rotation = "planar fit")
    ),
    data.frame(1:3)[0])
})

test_that("wresid is skipped if used_rotation column is missing", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(w_unrot = c(0.001, 0.4)),
                 filters = "wresid", used_rotation = TRUE)
    ),
    data.frame(1:2)[0])
})

test_that("wresid is skipped if used_rotation vals not recognized", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(
        w_unrot = c(0.001, 0.4),
        used_rotation = "wrong name"),
        filters = "wresid", used_rotation = TRUE)
    ),
    data.frame(1:2)[0])
})

test_that("wresid is skipped if used_rotation vars are not available", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(
        w_unrot = c(NA, 0.1, NA), # w_rot missing
        used_rotation = c("missing", "double", "planar fit")),
        filters = "wresid", used_rotation = TRUE)
    ),
    data.frame(1:3)[0])
})

test_that("used_rotation values are recognized partly", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(
        w_unrot = c(0.001, 0.4),
        used_rotation = c("wrong name", "double")),
        filters = "wresid", used_rotation = TRUE)
    ),
    set_attributes(data.frame(qc_ALL_wresid = c(0, 2)),
                   varnames = "qc_ALL_wresid"))
})

test_that("used_rotation values are handled correctly", {
  expect_equal(
    suppressMessages(
      extract_QC(data.frame(
        w_unrot = c(NA, 0.1, 0.4, 0.1, 0.4),
        w_rot = c(NA, 0.001, 0.2, 0.001, 0.2),
        used_rotation = c("wrong name", rep("double", 2), rep("planar fit", 2))),
        filters = "wresid", used_rotation = TRUE)
    ),
    set_attributes(data.frame(qc_ALL_wresid = c(0, rep(c(0, 2), 2))),
                   varnames = "qc_ALL_wresid"))
})
test_that("double rotation equals used_rotation double", {
  expect_equal(
    suppressMessages( # difference in messages due to different implementation
      extract_QC(data.frame(
        w_unrot = c(0.001, 0.4),
        used_rotation = "double"),
        filters = "wresid", used_rotation = TRUE)
    ),
    suppressMessages(
      extract_QC(data.frame(w_unrot = c(0.001, 0.4)), filters = "wresid")
    )
  )
})

test_that("planar fit rotation equals used_rotation planar fit", {
  expect_equal(
    suppressMessages( # difference in messages due to different implementation
      extract_QC(data.frame(
        w_rot = c(0.001, 0.4),
        used_rotation = "planar fit"),
        filters = "wresid", used_rotation = TRUE)
    ),
    suppressMessages(
      extract_QC(data.frame(w_rot = c(0.001, 0.4)),
                 filters = "wresid", rotation = "planar fit")
    )
  )
})

# Test interdep() ====
test_that("IRGA = 'en_closed' produces 2 columns", {
  expect_equal(interdep(0:2, IRGA = "en_closed"),
               set_attributes(data.frame(qc_H_interdep = c(0, 0, 1),
                                         qc_NEE_interdep = c(0, 0, 1)),
                              varnames = c("qc_H_interdep", "qc_NEE_interdep"),
                              units = c("-", "-")))
})

test_that("qc_H is ignored if IRGA = 'en_closed'", {
  expect_equal(interdep(0:2, 2, IRGA = "en_closed"),
               set_attributes(data.frame(qc_H_interdep = c(0, 0, 1),
                                         qc_NEE_interdep = c(0, 0, 1)),
                              varnames = c("qc_H_interdep", "qc_NEE_interdep"),
                              units = c("-", "-")))
})

test_that("IRGA = 'open' produces 3 columns", {
  expect_equal(interdep(0:2, 2:0, IRGA = "open"),
               set_attributes(data.frame(qc_H_interdep = c(0, 0, 1),
                                         qc_LE_interdep = c(1, 0, 0),
                                         qc_NEE_interdep = c(1, 0, 1)),
                              varnames = c("qc_H_interdep",
                                           "qc_LE_interdep",
                                           "qc_NEE_interdep"),
                              units = c("-", "-", "-")))
})

test_that("no recognized used_IRGA produces error", {
  expect_error(interdep(0:2, 2:0, used_IRGA = letters[1:3]),
               "no recognized type of IRGA in 'used_IRGA'")
})

test_that("not equal qc_LE and qc_H lengths produce error if IRGA = 'open'", {
  expect_error(interdep(0:2, 2, IRGA = "open"),
               "length of 'qc_LE' and 'qc_H' is not equal")
})

test_that("not equal qc_LE and used_IRGA lengths produce error", {
  expect_error(interdep(0:2, used_IRGA = letters[1:5]),
               "length of 'qc_LE' and 'used_IRGA' is not equal")
})

test_that("partially recognized used_IRGA works", {
  expect_equal(suppressMessages(
    interdep(rep(0:2, 3), rep(2:0, 3),
             used_IRGA = c(letters[1:3],
                           rep("en_closed", 3),
                           rep("open", 3)))),
    set_attributes(data.frame(
      qc_H_interdep = c(0, 0, 0, 0, 0, 1, 0, 0, 1),
      qc_LE_interdep = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
      qc_NEE_interdep = c(0, 0, 0, 0, 0, 1, 1, 0, 1)),
      varnames = c("qc_H_interdep",
                   "qc_LE_interdep",
                   "qc_NEE_interdep"),
      units = c("-", "-", "-")))
})

test_that("NULL input produces data frame with 0 rows", {
  expect_equal(interdep(NULL),
               set_attributes(
                 data.frame(qc_H_interdep = 0,
                            qc_NEE_interdep = 0)[0, ],
                 varnames = c("qc_H_interdep", "qc_NEE_interdep"),
                 units = c("-", "-")))
})

test_that("NA input produces flag correction", {
  expect_equal(interdep(c(rep(NA, 3), 0:2), c(2:0, rep(NA, 3)), IRGA = "open"),
               set_attributes(data.frame(
                 qc_H_interdep = c(1, 1, 1, 0, 0, 1),
                 qc_LE_interdep = c(1, 0, 0, 1, 1, 1),
                 qc_NEE_interdep = c(1, 1, 1, 1, 1, 1)),
                 varnames = c("qc_H_interdep",
                              "qc_LE_interdep",
                              "qc_NEE_interdep"),
                 units = c("-", "-", "-")))
})
# for testing message: evaluate_promise(fn())$messages