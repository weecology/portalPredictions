library(testthat)
library(yaml)
library(zoo)
context("checks that new forecasts are being added correctly")
data = yaml.load_file("../../tools/model.yaml")

all=read.csv(file.path('../../predictions', paste(as.character(as.Date(data$forecast_date)), "All", data$filename_suffix, ".csv", sep="")), na.strings = "")
allaic=read.csv(file.path('../../predictions', paste(as.character(as.Date(data$forecast_date)), "All", data$filename_suffix, "_model_aic.csv", sep="")), na.strings = "")
controls=read.csv(file.path('../../predictions', paste(as.character(as.Date(data$forecast_date)), "Controls", data$filename_suffix, ".csv", sep="")), na.strings = "")
controlsaic=read.csv(file.path('../../predictions', paste(as.character(as.Date(data$forecast_date)), "Controls", data$filename_suffix, "_model_aic.csv", sep="")), na.strings = "")

forecastnames = c("date", "forecastmonth",  "forecastyear",  "newmoonnumber",
                "currency", "model", "level", "species", "estimate",  "LowerPI", 
                "UpperPI",  "fit_start_newmoon", "fit_end_newmoon", "initial_newmoon")

aicnames = c("date", "currency", "model", "level", "species", "aic", "fit_start_newmoon",
             "fit_end_newmoon",  "initial_newmoon"  )

valid_currencies = c("abundance","biomass","energy")
valid_levels = paste('Plot',1:24,' ', sep = '')
valid_levels = c('All','Controls','FullExclosure','KratExclosure', valid_levels)
valid_species = c('total','BA','DM','DO','DS','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO','NA')

test_that("column names correct", {
  
  expect_true(all(colnames(all)==forecastnames))
  expect_true(all(colnames(controls)==forecastnames))
  expect_true(all(colnames(allaic)==aicnames))
  expect_true(all(colnames(controlsaic)==aicnames))
  
})

test_that("dates are valid", {
  expect_false(any(is.na(as.Date(all$date, '%Y-%m-%d'))))
  expect_false(any(is.na(as.Date(allaic$date, '%Y-%m-%d'))))
  expect_false(any(is.na(as.Date(controls$date, '%Y-%m-%d'))))
  expect_false(any(is.na(as.Date(controlsaic$date, '%Y-%m-%d'))))
})

test_that("years and months are valid", {
  expect_true(all(all$month %in% data$forecast_months))
  expect_true(all(all$year %in% data$forecast_years))
  expect_true(all(controls$month %in% data$forecast_months))
  expect_true(all(controls$year %in% data$forecast_years))
})

test_that("newmoons are valid", {
  expect_true(all(all$newmoonnumber %in% data$forecast_newmoons))
  expect_true(all(controls$newmoonnumber %in% data$forecast_newmoons))
})

test_that("currencies are valid", {
  expect_true(all(all$currency %in% valid_currencies))
  expect_true(all(allaic$currency %in% valid_currencies))
  expect_true(all(controls$currency %in% valid_currencies))
  expect_true(all(controlsaic$currency %in% valid_currencies))
})

test_that("level is valid", {
  expect_true(all(all$level %in% valid_levels))
  expect_true(all(allaic$level %in% valid_levels))
  expect_true(all(controls$level %in% valid_levels))
  expect_true(all(controlsaic$level %in% valid_levels))
})

test_that("species is valid", {
  expect_true(all(all$species %in% valid_species))
  expect_true(all(allaic$species %in% valid_species))
  expect_true(all(controls$species %in% valid_species))
  expect_true(all(controlsaic$species %in% valid_species))
})

test_that("no NAs in estimates, PIs, or AICs", {
  expect_false(any(is.na(all$estimate))) 
  expect_false(any(is.na(all$LowerPI))) 
  expect_false(any(is.na(all$UpperPI)))
  expect_false(any(is.na(allaic$aic)))
  expect_false(any(is.na(controls$estimate))) 
  expect_false(any(is.na(controls$LowerPI))) 
  expect_false(any(is.na(controls$UpperPI)))
  expect_false(any(is.na(controlsaic$aic)))
})

test_that('newmoon columns are integers only', {
  expect_true(is.integer(all$fit_start_newmoon)) 
  expect_true(is.integer(all$fit_end_newmoon)) 
  expect_true(is.integer(all$initial_newmoon)) 
  expect_false(any(is.na(all$fit_start_newmoon))) 
  expect_false(any(is.na(all$fit_end_newmoon))) 
  expect_false(any(is.na(all$initial_newmoon))) 
  expect_true(is.integer(controls$fit_start_newmoon)) 
  expect_true(is.integer(controls$fit_end_newmoon)) 
  expect_true(is.integer(controls$initial_newmoon)) 
  expect_false(any(is.na(controls$fit_start_newmoon))) 
  expect_false(any(is.na(controls$fit_end_newmoon))) 
  expect_false(any(is.na(controls$initial_newmoon)))
})
  
test_that("no forecasts duplicated", {

  expect_true(sum(duplicated(all[,1:8]))==0)
  expect_true(sum(duplicated(controls[,1:8]))==0)
  
})
