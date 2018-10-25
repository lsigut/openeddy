# NAs in x and thr lead to an error and do not have to be checked

library(openeddy)
context("Quality checking output")

set_attributes <- function(x) {
  attributes(x) <- list(varnames = "test", units = "-")
  return(x)
}

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

# apply_thr returns integers
