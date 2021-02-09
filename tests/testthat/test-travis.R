library(testthat)

context("checks that forecasts exist for use")
print("IS THIS THING ON?!?")
print(list.files())

test_that("cast metadata", {
  metadata <- read.csv("casts/casts_metadata.csv")
  expect_true(is.integer(metadata$cast_id))
})
