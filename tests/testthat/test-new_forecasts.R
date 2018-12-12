library(testthat)

context("checks that new forecasts are being added correctly")
model_metadata = yaml::yaml.load_file("../../data/metadata.yaml")

forecasts=read.csv(file.path('../../predictions', paste(as.character(as.Date(metadata$forecast_date)), 
                                        metadata$filename_suffix, ".csv", sep="")), na.strings = "")
forecastaics=read.csv(file.path('../../predictions', paste(as.character(as.Date(metadata$forecast_date)), 
                                        metadata$filename_suffix, "_model_aic.csv", sep="")), na.strings = "")

forecastnames = c("date", "forecastmonth",  "forecastyear",  "newmoonnumber",
                "currency", "model", "level", "species", "estimate",  "LowerPI", 
                "UpperPI",  "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")

aicnames = c("date", "currency", "model", "level", "species", "aic", "fit_start_newmoon",
             "fit_end_newmoon",  "initial_newmoon"  )

valid_currencies = c("abundance","biomass","energy")
valid_levels = paste('Plot',1:24,' ', sep = '')
valid_levels = c('All','Controls','FullExclosure','KratExclosure', valid_levels)
valid_species = c('total','BA','DM','DO','DS','OL','OT','PB','PE','PF','PH',
                  'PL','PM','PP','RF','RM','RO','SF','SH','SO','NA')

test_that("column names correct", {
  
  expect_true(all(colnames(forecasts)==forecastnames))
  expect_true(all(colnames(forecastaics)==aicnames))
  
})

test_that("dates are valid", {
  expect_false(any(is.na(as.Date(forecasts$date, '%Y-%m-%d'))))
  expect_false(any(is.na(as.Date(forecastaics$date, '%Y-%m-%d'))))
})

test_that("years and months are valid", {
  expect_true(all(forecasts$month %in% metadata$rodent_forecast_months))
  expect_true(all(forecasts$year %in% metadata$rodent_forecast_years))
})

test_that("newmoons are valid", {
  expect_true(all(forecasts$newmoonnumber %in% metadata$rodent_forecast_newmoons))
})

test_that("currencies are valid", {
  expect_true(all(forecasts$currency %in% valid_currencies))
  expect_true(all(forecastaics$currency %in% valid_currencies))
})

test_that("level is valid", {
  expect_true(all(forecasts$level %in% valid_levels))
  expect_true(all(forecastaics$level %in% valid_levels))
})

test_that("species is valid", {
  expect_true(all(forecasts$species %in% valid_species))
  expect_true(all(forecastaics$species %in% valid_species))
})

test_that("no NAs in estimates, PIs, or AICs", {
  expect_false(any(is.na(forecasts$estimate))) 
  expect_false(any(is.na(forecasts$LowerPI))) 
  expect_false(any(is.na(forecasts$UpperPI)))
  expect_false(any(is.na(forecastaics$aic)))
})

test_that('newmoon columns are integers only', {
  expect_true(is.integer(forecasts$fit_start_newmoon)) 
  expect_true(is.integer(forecasts$fit_end_newmoon)) 
  expect_true(is.integer(forecasts$initial_newmoon)) 
  expect_false(any(is.na(forecasts$fit_start_newmoon))) 
  expect_false(any(is.na(forecasts$fit_end_newmoon))) 
  expect_false(any(is.na(forecasts$initial_newmoon))) 
})
  
test_that("no forecasts duplicated", {
  expect_true(sum(duplicated(forecasts[,1:8]))==0)
  
})
