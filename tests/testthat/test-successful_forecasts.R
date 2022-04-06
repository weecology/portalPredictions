library(testthat)
library(portalcasting)

context("checks that a production pipeline has been setup and run correctly")

test_that("folders exist as needed",{
  fnames <- list.files()
  expect_equal(all(c("casts", "data", "models", "raw", "tmp", "fits") %in% fnames), TRUE)
})

test_that("dir_config is present",{
  fnames <- list.files()
  expect_equal("dir_config.yaml" %in% fnames, TRUE)
})

test_that("cast_metadata file", {
  expect_is(read_cast_metadata(), "list")
})

test_that("new casts have been made", {
  expect_true(Sys.Date() == as.Date(read_metadata()$time$cast_date))
})
