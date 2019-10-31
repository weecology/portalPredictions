context("checks that repository is building and updating correctly")

test_that("data have been updated", {
  
  expect_true(Sys.Date()==as.Date(read_metadata("../../")$cast_date))
  
})
