library(testthat)
context("checks that new forecasts are being added correctly")
forecast_date = 
all=read.csv(file.path('predictions', paste(as.character(forecast_date), "All", "forecasts", ".csv", sep="")))
allaic=read.csv(file.path('predictions', paste(as.character(forecast_date), "All", "forecasts", "_model_aic.csv", sep="")))
controls=read.csv(file.path('predictions', paste(as.character(forecast_date), "All", "forecasts", ".csv", sep="")))
controlsaic=read.csv(file.path('predictions', paste(as.character(forecast_date), "All", "forecasts", "_model_aic.csv", sep="")))

forecastnames = c("date", "forecastmonth",  "forecastyear",  "newmoonnumber",
                "currency", "model", "level", "species", "estimate",  "LowerPI", 
                "UpperPI",  "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")

aicnames = c("date", "currency", "model", "level", "species", "aic", "fit_start_newmoon",
             "fit_end_newmoon",  "initial_newmoon"  )

test_that("column names correct", {
  
  expect_true(all(colnames(all)==forecastnames))
  expect_true(all(colnames(controls)==forecastnames))
  expect_true(all(colnames(allaic)==aicnames))
  expect_true(all(colnames(controlsaic)==aicnames))
  
})

test_that("no forecasts duplicated", {

  expect_true(sum(duplicated(all[,1:8]))==0)
  expect_true(sum(duplicated(controls[,1:8]))==0)
  
})
