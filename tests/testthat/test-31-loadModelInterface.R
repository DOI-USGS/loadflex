context("load model interface")
test_that("validLoadModelInterface reports validity for all base load model types", {
  
  # Verbose mode is nice if you're running this manually
  verbose <- FALSE
  
  # Made-up data and metadata
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10+rnorm(10,0,0.2),datetime=seq(as.Date("2000-05-01"), by="7 days", length.out=10))
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  
  # loadModel
  load.model <- loadModel(
    inner.fit.function=function(training.data) { lm(conc ~ discharge, data=training.data) }, 
    pred.format="conc", data=mydat, metadata=mymd)
  expect_true(validLoadModelInterface(load.model, verbose=verbose))
  
  # loadLm
  load.lm <- loadLm(
    formula=log(conc) ~ discharge, 
    pred.format="conc", data=mydat, metadata=mymd)
  expect_true(validLoadModelInterface(load.lm, verbose=verbose))
  
  # loadReg2 - segfaults on travis
  # library(rloadest)
  # load.reg2 <- loadReg2(loadReg(
  #   conc ~ model(2), data = mydat,
  #   flow = "discharge", dates = "datetime", conc.units="mg/L"))
  # expect_true(validLoadModelInterface(load.reg2, verbose=verbose))
  
  # loadInterp
  load.interp <- loadInterp(
    interp.format="conc", data=mydat, 
    interp.function=linearInterpolation, metadata=mymd)
  expect_true(validLoadModelInterface(load.interp, verbose=verbose))
  
  # loadComp
  load.comp <- loadComp(
    reg.model=load.lm, interp.format="flux", 
    interp.data=mydat, interp.function=linearInterpolation, n.iter = 0)
  expect_true(validLoadModelInterface(load.comp, verbose=verbose))
  
})