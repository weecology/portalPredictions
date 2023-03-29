library(testthat)

context("checks that forecasts exist for use")

test_that("cast metadata", {
  metadata <- read.csv("forecasts/casts_metadata.csv")
  expect_true(is.numeric(metadata$cast_id))
})
