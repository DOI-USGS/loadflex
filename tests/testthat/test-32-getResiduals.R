context("getResiduals")
test_that("getResiduals works", {
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # get residuals from fitting data
  expect_equal(dim(getResiduals(reg.model)), c(27,2)) 
  
  # get residuals from new data
  app2.calib2 <- transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  expect_equal(dim(getResiduals(reg.model, newdata=app2.calib2)), c(32,2))
  
  # fail elegantly to get residuals from data that don't have observations
  app2.calib3 <- app2.calib2[c("DATES","TIMES","FLOW","Period")]
  expect_error(getResiduals(reg.model, newdata=app2.calib3), "'Atrazine'")
  
  # check that flux residuals and conc residuals can be calculated from one
  # another if computed as absolutes in linear space
  residmetadata <- updateMetadata(getMetadata(reg.model), load.rate="Resid", constituent="Resid")
  abslinflux <- getResiduals(reg.model, "flux", abs.or.rel.resids="absolute", use.log=FALSE)
  abslinconc <- getResiduals(reg.model, "conc", abs.or.rel.resids="absolute", use.log=FALSE)
  expect_equal(observeSolute(data.frame(FLOW=simpledata$FLOW, abslinflux), "conc", meta=residmetadata, calculate=TRUE),
               abslinconc$Resid)
  expect_equal(observeSolute(data.frame(FLOW=simpledata$FLOW, abslinconc), "flux", meta=residmetadata, calculate=TRUE),
               abslinflux$Resid)
  
  # check that flux & conc residuals equal one another if computed as absolutes
  # in log space, or as relatives in linear space
  abslinflux <- getResiduals(reg.model, "flux", abs.or.rel.resids="absolute", use.log=TRUE)
  abslinconc <- getResiduals(reg.model, "conc", abs.or.rel.resids="absolute", use.log=TRUE)
  expect_equal(abslinflux$Resid, abslinconc$Resid)
  abslinflux <- getResiduals(reg.model, "flux", abs.or.rel.resids="relative", use.log=FALSE)
  abslinconc <- getResiduals(reg.model, "conc", abs.or.rel.resids="relative", use.log=FALSE)
  expect_equal(abslinflux$Resid, abslinconc$Resid)
  
  # it's pretty strange to use relative AND logged flux & conc residuals. it
  # can be done, but  i don't recommend it
  abslinflux <- getResiduals(reg.model, "flux", abs.or.rel.resids="relative", use.log=TRUE)
  abslinconc <- getResiduals(reg.model, "conc", abs.or.rel.resids="relative", use.log=TRUE)
  #plot(abslinconc$Resid ~ abslinflux$Resid) # see how strange?
})