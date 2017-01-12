context("unit.conversions")

tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("validMetadataUnits works", {

  expect_true(validMetadataUnits("mg L^-1"))
  expect_true(validMetadataUnits("ft^3 s^-1"))
  expect_true(validMetadataUnits("kg"))
  expect_true(validMetadataUnits("colonies d^-1"))
  expect_true(validMetadataUnits("kg d^-1"))
  expect_true(validMetadataUnits("d^-1 kg"))
  expect_true(validMetadataUnits("colonies s^-1"))
  expect_true(validMetadataUnits("ug L^-1"))
  expect_true(validMetadataUnits("mg ft^-3"))
  expect_true(validMetadataUnits("g dL^-1"))
  
  expect_false(validMetadataUnits("Mg^-2"))
  expect_false(validMetadataUnits("d"))
  expect_false(validMetadataUnits("colonies sec^-1"))
  expect_false(validMetadataUnits("cfs"))
  expect_false(validMetadataUnits("cubic meters per second"))
  
})

test_that("translateFreeformToUnitted works", {
  
  expect_equal(loadflex:::translateFreeformToUnitted("colonies /L"), "colonies L^-1")
  expect_equal(loadflex:::translateFreeformToUnitted("mg per dL"), "mg dL^-1")
  expect_equal(loadflex:::translateFreeformToUnitted("cfs"), "ft^3 s^-1")
  expect_equal(loadflex:::translateFreeformToUnitted("cubic meters per second"), "m^3 s^-1")
  
})

test_that("flowUnitsConversion works",m {
  expect_silent(verify_units(
    flowUnitsConversion(old.units='ft^3 s^-1', new.units='dL d^-1', attach.units = TRUE),
    expected.units = 'dL d^-1 ft^-3 s'))
  expect_error(flowUnitsConversion(old.units='g m^-3', new.units='mg L^-1')) # error because not flow
})

test_that("flowconcToFluxConversion works", {
  cf1 <- flowconcToFluxConversion(flow.units = "ft^3 d^-1", conc.units = "mg L^-1", load.rate.units = "kg d^-1", attach.units = TRUE)
  expect_equivalent(v(cf1), 2.8317e-05)
  expect_equal(get_units(cf1), "kg L ft^-3 mg^-1")
  
  cf2 <- flowconcToFluxConversion(flow.units = "ft^3 d^-1", conc.units = "mg L^-1", load.rate.units = "kg d^-1", attach.units = FALSE)
  expect_equivalent(cf2, 2.8317e-05)
  expect_equal(get_units(cf2), NA)

  # Comparisons to the sister rloadest function
  expect_equal(flowconcToFluxConversion("cfs","ug/l","kg/d"), rloadest::loadConvFactor("cfs","ug/L","kg"))
  expect_equal(flowconcToFluxConversion("cfs","colonies/dL","million colonies per d"), rloadest::loadConvFactor("cfs","col/dL","million colonies"))
})

test_that("observeSolute generates fluxes with the expected units & format", {
  obs <- data.frame(MyConc=1:10, MyFlow=rep(10,10))
  row.names(obs) <- paste(11:20)
  md <- updateMetadata(exampleMetadata(), constituent="MyConc", flow="MyFlow", dates="none",
                       flow.units="cms", conc.units="mg/l", load.units="mg", load.rate.units="mg/day")
  
  # See whether observeSolute can calculate fluxes
  # cms * mg/L = m^3 mg / (L s). To get to mg/day, multiply by 1000 L/m^3 and 60*60*24 s/day
  expect_that(observeSolute(obs, "flux", md), equals(obs$MyConc*obs$MyFlow*1000*60*60*24))
  expect_that(get_units(observeSolute(obs, "flux", md, attach.units=TRUE)), equals("mg d^-1"))
  
  # If we're also converting to kg, divide by 1000000
  expect_that(observeSolute(obs, "flux", updateMetadata(md, load.units="kg", load.rate.units="kg/day"), attach.units=TRUE), 
              equals(u(obs$MyConc*obs$MyFlow*1000*60*60*24/1000000, units="kg d^-1")))
  # If we're converting from flow units of cfs, we need 28.317 L/ft^3 instead of 1000 L/m^3
  expect_that(observeSolute(obs, "flux", updateMetadata(md, flow.units="cfs"), attach.units=TRUE), 
              equals(u(obs$MyConc*obs$MyFlow*28.317*60*60*24, units="mg d^-1")))

  # See whether observeSolute can find fluxes
  obs$MyFlux <- 7 #intentionally wrong
  expect_equivalent(observeSolute(obs, "flux", updateMetadata(md, load.rate="MyFlux"), calculate=FALSE), rep(7, nrow(obs)))
  
  # Check the formatting
  expect_equal(observeSolute(obs, "flux", md, calculate=TRUE), observeSolute(obs, "flux", md))
  expect_equal(observeSolute(obs, "flux", md, attach.units=FALSE), as.numeric(observeSolute(obs, "flux", md, attach.units=TRUE)))
  expect_is(observeSolute(obs, "flux", md), "numeric")
  expect_that(names(observeSolute(obs, "flux", md)), equals(NULL))
})

test_that("observeSolute generates concentrations with the expected units & format", {
  obs <- data.frame(MyConc=1:10, MyFlow=rep(10,10), MyFlux=2) # intentionally inconsistent
  row.names(obs) <- paste(11:20)
  md <- updateMetadata(exampleMetadata(), constituent="MyConc", flow="MyFlow", load.rate="MyFlux", dates="none",
                       flow.units="cms", conc.units="mg/l", load.units="mg", load.rate.units="mg/day")
  
  # Now observeSolute
  expect_equal(observeSolute(obs, "conc", md, calculate=TRUE, attach.units=TRUE), u(obs$MyFlux / obs$MyFlow / (1000*60*60*24), units="mg L^-1"))
  expect_equal(observeSolute(obs, "conc", md, calculate=TRUE, attach.units=TRUE), formatPreds(obs$MyFlux / obs$MyFlow, from.format="flux/flow", to.format="conc", metadata=md, attach.units=TRUE))
  expect_equal(observeSolute(obs, "conc", md, calculate=TRUE, attach.units=FALSE), formatPreds(obs$MyFlux / obs$MyFlow, from.format="flux/flow", to.format="conc", metadata=md, attach.units=FALSE))
  
  # the default is !attach.units and !calculate
  expect_equal(observeSolute(obs, "conc", md), obs$MyConc) 
  expect_equal(observeSolute(obs, "conc", md, attach.units=FALSE), observeSolute(obs, "conc", md))
  expect_equal(observeSolute(obs, "conc", md, calculate=FALSE), observeSolute(obs, "conc", md))
})

test_that("formatPreds gets predictions into the right format", {
  obs <- transform(data.frame(MyConc=1:10, MyFlow=rep(10,10)), MyFlux=MyConc*MyFlow*rloadest::loadConvFactor("cms", "mg/l", "mg") )
  row.names(obs) <- paste(11:20)
  md <- updateMetadata(exampleMetadata(), constituent="MyConc", flow="MyFlow", load.rate="MyFlux", dates="none",
                       flow.units="cms", conc.units="mg/l", load.units="mg", load.rate.units="mg/day")
  
  # get conc from a variety of inputs
  expect_equal(formatPreds(preds=obs$MyConc, from.format="conc", to.format="conc", newdata=NA, metadata=md), obs$MyConc)
  expect_equal(formatPreds(preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="conc", newdata=obs, metadata=md), obs$MyConc)
  expect_equal(formatPreds(preds=obs$MyFlux/obs$MyFlow, from.format="flux/flow", to.format="conc", newdata=obs, metadata=md), obs$MyConc)
  expect_equal(formatPreds(preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md), obs$MyConc)
  
  # get flux from a variety of inputs
  expect_equal(formatPreds(preds=obs$MyConc, from.format="conc", to.format="flux", newdata=obs, metadata=md), obs$MyFlux)
  expect_equal(formatPreds(preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="flux", newdata=NA, metadata=md), obs$MyFlux)
  expect_equal(formatPreds(preds=obs$MyFlux/obs$MyFlow, from.format="flux/flow", to.format="flux", newdata=obs, metadata=md), obs$MyFlux)
  expect_equal(formatPreds(preds=obs$MyFlux, from.format="flux", to.format="flux", newdata=NA, metadata=md), obs$MyFlux)
  
  # Check that units get attached when requested
  expect_equal(formatPreds(preds=obs$MyFlux, from.format="flux", to.format="conc", newdata=obs, metadata=md, attach.units=TRUE), u(obs$MyConc, "mg L^-1"))
  expect_equal(formatPreds(preds=obs$MyConc*obs$MyFlow, from.format="conc*flow", to.format="flux", newdata=NA, metadata=md, attach.units=TRUE), u(obs$MyFlux, units="mg d^-1"))
})
