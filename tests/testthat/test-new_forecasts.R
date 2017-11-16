library(testthat)
context("checks that new forecasts are being added correctly")
latestfiles=tail(list.files("../../predictions", full.names = TRUE),4)
all=read.csv(latestfiles[1])
allaic=read.csv(latestfiles[2])
controls=read.csv(latestfiles[3])
controlsaic=read.csv(latestfiles[4])

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

  expect_true(sum(duplicated(all))==0)
  expect_true(sum(duplicated(controls))==0)
  
})
