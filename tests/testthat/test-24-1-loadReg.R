context("loadReg() is defined within rloadest. The functions required to implement loadModelInterface are defined in loadReg.R within the current package.")
tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

#' loadReg() is defined within rloadest. The functions required to implement
#' loadModelInterface are defined in loadReg.R within the current package.
library(rloadest)

test_that("loadReg models can be created", {
  
  suppressWarnings(rm(simpledata)) #make sure it's not defined elsewhere
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  
  # Fit the model
  load.model <- loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L")
  
  expect_is(load.model, "loadReg")
  
})

test_that("metadata can be extracted from loadReg models", {

  library(rloadest)
  # Sample data & model
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  load.model <- loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L")
  
  expect_equal(getMetadata(load.model), 
               metadata(const="Atrazine", flow="FLOW", load.rate="", dates="DATES", conc.units="mg L^-1", 
                        flow.units="s^-1 ft^3", load.units="kg", load.rate.units="kg/day", station=""))
  
})
 

test_that("resampleCoefficients.loadReg looks OK", {
  
  # Sample data & model
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], Period=seasons(DATES,breaks=c("Apr", "Jul")))
  load.model <- loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L")
  
  # resample 1000 times and plot the resampled coefficients
  new_coefs <- setNames(data.frame(t(replicate(n=1000, coef(resampleCoefficients.loadReg(load.model, "flux"))))), c("intercept","period","flow","periodflow"))
  #print(ggplot(new_coefs, aes(x=intercept, y=period, color=flow)) + geom_point() + theme_bw())
  # repeat, but for flux
  new_coefs <- setNames(data.frame(t(replicate(n=1000, coef(resampleCoefficients.loadReg(load.model, "conc"), which="conc")))), c("intercept","period","flow","periodflow"))
  #print(ggplot(new_coefs, aes(x=intercept, y=period, color=flow)) + geom_point() + theme_bw())
  #   library(lattice)
  #   cloud(intercept ~ dtsimple * discharge, data=new_coefs, alpha=0.6)
  
  # check against covariance matrix
  SCV <- load.model$cfit$SCV
  NPAR <- load.model$cfit$NPAR
  GAMMA <- SCV[1:NPAR,NPAR+1]/SCV[NPAR+1,NPAR+1]
  C <- SCV[1:NPAR,1:NPAR] - SCV[NPAR+1,NPAR+1]*(GAMMA %*% t(GAMMA))
  dimnames(C) <- list(c("intercept","period","flow","periodflow"), c("intercept","period","flow","periodflow"))
  S2 <- load.model$cfit$PARMLE[NPAR+1]/(1+load.model$cfit$BIAS[NPAR+1])
  #print(cov.scaled <- S2*C)
  #expect_manual_OK("resampled coefficients follow the expected covariance structure")
})