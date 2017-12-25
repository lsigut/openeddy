# NAs in x and thr lead to an error and do not have to be checked

library(openeddy)
context("Quality checking output")

set_attributes <- function(x) {
  attributes(x) <- list(varnames = "test", units = "-")
  return(x)
}

test_that("apply_thr flags higher correctly", {
  expect_equal(apply_thr(1:3, c(3, 6), "test"),
               set_attributes(rep(0, 3)))
  expect_equal(apply_thr(4:6, c(3, 6), "test"),
               set_attributes(rep(1, 3)))
  expect_equal(apply_thr(7:9, c(3, 6), "test"),
               set_attributes(rep(2, 3)))
})

test_that("apply_thr flags lower correctly", {
  expect_equal(apply_thr(-9:-7, c(-3, -6), "test", "lower"),
               set_attributes(rep(2, 3)))
  expect_equal(apply_thr(-6:-4, c(-3, -6), "test", "lower"),
               set_attributes(rep(1, 3)))
  expect_equal(apply_thr(-3:-1, c(-3, -6), "test", "lower"),
               set_attributes(rep(0, 3)))
})

test_that("both thr values identical work in apply_thr correctly", {
  expect_equal(apply_thr(c(2, 2.1), c(2, 2), "test"),
               set_attributes(c(0, 2)))
  expect_equal(apply_thr(c(2, 1.9), c(2, 2), "test", "lower"),
               set_attributes(c(0, 2)))
})

# apply_thr returns integers
