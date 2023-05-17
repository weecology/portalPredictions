library(testthat)
library(portalcasting)

context("checks that a production pipeline has been setup and run correctly")

test_that("folders exist as needed",{
  fnames <- list.files()
  expect_equal(all(c("app.R", "forecasts", "data", "models", "resources", "fits", "www") %in% fnames), TRUE)
})

test_that("dir_config is present",{
  fnames <- list.files()
  expect_equal("directory_configuration.yaml" %in% fnames, TRUE)
})

test_that("cast_metadata file", {
  expect_is(read_forecasts_metadata(), "data.frame")
})

test_that("new casts have been made", {
  expect_true(Sys.Date() == as.Date(read_metadata()$time$forecast_date))
})