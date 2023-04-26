library(testthat)

context("checks that forecasts exist for use")

test_that("cast metadata", {
  metadata <- read.csv("forecasts/forecasts_metadata.csv")
  expect_true(is.numeric(metadata$forecast_id))
})