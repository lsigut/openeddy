# NAs in x and thr lead to an error and do not have to be checked

set_attributes <- function(x) {
  attributes(x) <- list(varnames = "test", units = "-")
  return(x)
}

# Test apply_thr()
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

# Test flag_runs()
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



