tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("metadata can be initialized", {
  
  # new("metadata") does no error checking. This is not recommended for users.
  expect_is(new("metadata"), "metadata")
  
  # metadata() requires every argument except station and custom and tells 
  # you which you're missing. Those arguments with defaults need not be amended;
  # the resulting object will be valid as long as all others are non-empty.
  expect_error(metadata(), 'constituent must be a non-empty string')
  expect_is(metadata(validate=FALSE), "metadata")
  expect_error(metadata(constituent=""), 'flow must be a non-empty string')
  expect_error(metadata(constituent="NO3", flow="FLOW"), 'dates must be a non-empty string')
  expect_error(metadata(constituent="NO3", flow="FLOW", dates="DATE"), 'conc.units are invalid')
  
  # metadata doesn't require argument tags as long as you get everything in
  # the right order
  expect_is(metadata(
    "NO3", "Q", "NO3_FLUX", "DATE", "mg/L", "cms", "kg", "kg/day",
    station="Lamprey River @ Wiswall Dam, Durham, New Hampshire", 
    custom=list(a="anything you want goes here", b=1:10)), "metadata")
  
  # After all arguments are supplied, they're still checked for validity
  expect_error(metadata("", "Q", "NO3_FLUX", "DATE", "mg/L", "cms", "kg", "kg/day"), "constituent must be a non-empty string")
  expect_error(metadata("NO3", "Q", "NO3_FLUX", "DATE", "mg/L", "cms", "kg", "kg/month"), "unexpected unit 'month'")
})

test_that("metadata can be revised (with validation)", {
  data(eg_metadata)
  # updateMetadata won't create a metadata object
  expect_error(updateMetadata(
    constituent="NO3", flow="Q", load.rate="NO3_FLUX", dates="DATE",
    conc.units="mg/L", flow.units="cms", load.units="kg", load.rate.units="kg/day",
    station="Lamprey River @ Wiswall Dam, Durham, New Hampshire", 
    custom=list(a="anything you want goes here", b=1:10)), 'argument "metadata" is missing, with no default')
  expect_is(updateMetadata(eg_metadata, constituent="solute", flow="FLOW", dates="DATES"), "metadata")
  expect_is(updateMetadata(eg_metadata, constituent="TSS", flow="FLOW", dates="DATES", flow.units="cfs", load.units="kg", conc.units="mg/L"), "metadata")
  expect_is(updateMetadata(updateMetadata(eg_metadata, constituent="solute", flow="FLOW", dates="DATES"), load.units="g", load.rate.units="g/day"), "metadata")
  expect_equal(updateMetadata(eg_metadata, constituent="solute", flow="FLOW", dates="DATES")@flow.units, "ft^3 s^-1")
  
  # Revisions are checked within updateMetadata
  expect_error(updateMetadata(eg_metadata, nonsense=""), "unrecognized metadata element: nonsense")
  expect_error(updateMetadata(eg_metadata, constituent=""), "constituent must be a non-empty string")
  expect_error(updateMetadata(eg_metadata, flow=""), "flow must be a non-empty string")
  expect_error(updateMetadata(eg_metadata, dates=""), "dates must be a non-empty string")
  expect_error(updateMetadata(eg_metadata, flow.units="w"), "unexpected unit")
  expect_error(updateMetadata(eg_metadata, conc.units="x"), "unexpected unit")
  expect_error(updateMetadata(eg_metadata, load.units="y"), "load.units are invalid")
})


test_that("metadata units are standardized during creation/update", {
  expect_equal(metadata("NO3", "Q", "NO3_FLUX", "DATE", "mg/L", "cms", "kg", "kg/d"),
               metadata("NO3", "Q", "NO3_FLUX", "DATE", "mg L^-1", "m^3 s^-1", "kg", "d^-1 kg^1  "))
})


test_that("metadata can be accessed by getCol", {
  data(eg_metadata)
  md <- eg_metadata
  da <- data.frame(NO3=1:3, Q=4:6, NO3_FLUX="hi", DATE=as.Date("2007-03-14"))
  
  # Confirm partial matching and case insensitivity
  expect_equal(getCol(md, da, "flux rate"), getCol(md, da, "flux r"))
  expect_equal(getCol(md, da, "con"), getCol(md, da, "conc"))
  
  # Confirm error checking for ambiguity and non-matches
  expect_warning(getCol(md, da, "flux"), "you specified field=='flux', but the nearest metadata match is 'flux rate'") # There is no flux, only flux rate, so warning is appropriate
  expect_error(getCol(md, da, "fl"), "'field' should be one of") # There are two matches for "fl"; error for ambiguity is appropriate
  expect_error(getCol(md, da, "meatball"), "'field' should be one of")
  expect_error(getCol(md, da, "Station"), "'field' should be one of")
  
  # Confirm error checking for mismatch between data & metadata
  expect_error(getCol(md, da[,c(2,3,4)], "c"), "data does not contain the expected")
  expect_error(getCol(md, da[,c(1,3,4)], "flo"), "data does not contain the expected")
  expect_error(getCol(md, da[,c(1,2,4)], "flux rate"), "data does not contain the expected")
  expect_error(getCol(md, da[,c(1,2,3)], "d"), "data does not contain the expected")  
})

test_that("metadata can be accessed by getUnits", {
  data(eg_metadata)
  md <- eg_metadata
  
  # Confirm partial matching and case insensitivity
  expect_equal(getUnits(md, "con"), getUnits(md, "conc"))
  expect_equal(getUnits(md, "flux rate"), getUnits(md, "flux r"))
  
  # Confirm error checking for ambiguity and non-matches
  expect_error(getUnits(md, "fl"), "should be one of")
  expect_error(getUnits(md, "spaghetti"), "should be one of")
  expect_error(getUnits(md, "Custom"), "should be one of")
  
  # Different formats for different packages
  expect_equal(getUnits(md, 'conc', format=NA), 'mg L^-1')
  expect_equal(getUnits(md, 'conc', format='rloadest'), 'mg/L')
  expect_equal(getUnits(md, 'conc', format='EGRET'), 'mg/l')
  expect_equal(getUnits(md, 'flux rate', format='EGRET'), 'kg d^-1')
  expect_error(getUnits(md, 'flux rate', format='LOADEST'), 'unrecognized unit format')
})

test_that("metadata can be accessed by getInfo", {
  data(eg_metadata)
  md <- eg_metadata
  
  # Confirm partial matching and case insensitivity
  expect_equal(getInfo(md, "da"), getInfo(md, "dates"))
  expect_equal(getInfo(md, "cu", FALSE), getInfo(md, "custom", FALSE))
  
  # Confirm error checking for ambiguity and non-matches
  expect_error(getInfo(md, "fl"), "should be one of")
  expect_error(getInfo(md, "spaghetti"), "should be one of")
  
  # Confirm error checking for empty values
  expect_error(getInfo(md, "site.id", TRUE), "must be a non-empty string")
  expect_equal(getInfo(md, "site.id", FALSE), '')
  expect_equal(getInfo(md, "load.rate", TRUE), 'NO3_FLUX')
  expect_equal(getInfo(md, "load.rate", FALSE), 'NO3_FLUX')
})

test_that("metadata can be displayed", {
  data(eg_metadata)
  expect_output(show(eg_metadata), "constituent(.*)flow(.*)dates")
})

test_that("metadata can be tested for equality", {
  data(eg_metadata)
  md <- updateMetadata(eg_metadata, flow="Q", custom=NULL)
  expect_true(md == md)
  expect_true(md == updateMetadata(md, flow="Q"))
  expect_false(md == updateMetadata(md, flow="discharge"))
})

