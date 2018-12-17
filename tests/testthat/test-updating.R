context("checks that repository is building and updating correctly")
model_metadata = yaml::yaml.load_file("../../data/metadata.yaml")

test_that("data have been updated", {
  
  expect_true(Sys.Date()==as.Date(model_metadata$forecast_date))
  
})
