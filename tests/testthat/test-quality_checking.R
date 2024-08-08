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