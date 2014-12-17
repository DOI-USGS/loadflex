tryCatch({source("inst/tests/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

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
    
  # exampleMetadata can be used to construct a valid metadata object without 
  # typing in arguments. It's only useful for testing, of course, not for use
  # with actual data.
  expect_is(exampleMetadata(), "metadata")  
  expect_true(validObject(exampleMetadata()))
})

test_that("metadata can be revised (with validation)", {
  # updateMetadata won't create a metadata object
  expect_error(updateMetadata(
    constituent="NO3", flow="Q", load.rate="NO3_FLUX", dates="DATE",
    conc.units="mg/L", flow.units="cms", load.units="kg", load.rate.units="kg/day",
    station="Lamprey River @ Wiswall Dam, Durham, New Hampshire", 
    custom=list(a="anything you want goes here", b=1:10)), 'argument "metadata" is missing, with no default')
  expect_is(updateMetadata(exampleMetadata(), constituent="solute", flow="FLOW", dates="DATES"), "metadata")
  expect_is(updateMetadata(exampleMetadata(), constituent="TSS", flow="FLOW", dates="DATES", flow.units="cfs", load.units="kg", conc.units="mg/L"), "metadata")
  expect_is(updateMetadata(updateMetadata(exampleMetadata(), constituent="solute", flow="FLOW", dates="DATES"), load.units="g", load.rate.units="g/day"), "metadata")
  expect_equal(updateMetadata(exampleMetadata(), constituent="solute", flow="FLOW", dates="DATES")@flow.units, "m^3 s^-1")
  
  # Revisions are checked within updateMetadata
  md <- exampleMetadata()
  expect_error(updateMetadata(md, nonsense=""), "unrecognized metadata element: nonsense")
  expect_error(updateMetadata(md, constituent=""), "constituent must be a non-empty string")
  expect_error(updateMetadata(md, flow=""), "flow must be a non-empty string")
  expect_error(updateMetadata(md, dates=""), "dates must be a non-empty string")
  expect_error(updateMetadata(md, flow.units="w"), "unexpected unit")
  expect_error(updateMetadata(md, conc.units="x"), "unexpected unit")
  expect_error(updateMetadata(md, load.units="y"), "unexpected unit")
})


test_that("metadata units are standardized during creation/update", {
  expect_equal(metadata("NO3", "Q", "NO3_FLUX", "DATE", "mg/L", "cms", "kg", "kg/d"),
               metadata("NO3", "Q", "NO3_FLUX", "DATE", "mg L^-1", "m^3 s^-1", "kg", "d^-1 kg^1  "))
})


test_that("metadata can be accessed by getCol", {
  md <- exampleMetadata()
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
  md <- exampleMetadata()
  
  # Confirm partial matching and case insensitivity
  expect_equal(getUnits(md, "con"), getUnits(md, "conc"))
  expect_equal(getUnits(md, "flux rate"), getUnits(md, "flux r"))
  
  # Confirm error checking for ambiguity and non-matches
  expect_error(getUnits(md, "fl"))
  expect_error(getUnits(md, "spaghetti"))
  expect_error(getUnits(md, "Custom"))
})

test_that("metadata can be accessed by getInfo", {
  md <- exampleMetadata()
  
  # Confirm partial matching and case insensitivity
  expect_equal(getInfo(md, "cu"), getInfo(md, "custom"))
  expect_equal(getInfo(md, "s"), getInfo(md, "sta"))
  
  # Confirm error checking for ambiguity and non-matches
  expect_error(getInfo(md, "fl"))
  expect_error(getInfo(md, "spaghetti"))
})

test_that("metadata can be displayed", {
  expect_output(show(exampleMetadata()), "constituent(.*)flow(.*)dates")
})

test_that("metadata can be tested for equality", {
  md <- updateMetadata(exampleMetadata(), flow="Q", custom=NULL)
  expect_true(md == md)
  expect_true(md == updateMetadata(md, flow="Q"))
  expect_false(md == updateMetadata(md, flow="discharge"))
})

