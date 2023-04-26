library(testthat)

context("checks that github repository is updated correctly")

# test that the remote github main branch has the newest forecasts
new_commit <- git2r::last_commit()

test_that("placeholder", {

  expect_true(grepl("Update forecasts: HiperGator Build",new_commit$summary))
  expect_true(grepl(Sys.Date(),new_commit$summary))
})