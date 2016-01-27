tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("isTimestepRegular works", {
  library(rloadest)
  simpledata <- app2.calib[-which(diff(app2.calib$DATES) < 7),]
  
  # Error handling should be up to the caller
  expect_error(isTimestepRegular(simpledata$DATES, hist=FALSE, handler=stop), "Time series is irregular")
  expect_warning(isTimestepRegular(simpledata$DATES, hist=FALSE, handler=warning), "Time series is irregular")
  expect_false(isTimestepRegular(simpledata$DATES, hist=FALSE, handler=function(e) {}))
  expect_false(isTimestepRegular(simpledata$DATES, hist=TRUE, handler=function(e) {}))
  expect_manual_OK("Histogram of timesteps makes sense")
  
  # Regular time steps should pass
  simpledata <- transform(simpledata, DATES=seq(DATES[1], DATES[length(DATES)], length.out=length(DATES)))
  expect_true(isTimestepRegular(simpledata$DATES, hist=TRUE, handler=function(e) {}))
  
  # Tolerance should be settable
  simpledata <- transform(simpledata, DATES=DATES + pmin(pmax(rnorm(length(DATES), 0, 0.1), -0.5), 0.5))
  expect_false(isTimestepRegular(simpledata$DATES, hist=TRUE, handler=function(e) {}))
  expect_true(isTimestepRegular(simpledata$DATES, hist=TRUE, tol = 1, handler=function(e) {}))
  
})


test_that("Durbin Watson tests are reasonable", {
  library(rloadest)
  simpledata <- transform(app2.calib[-which(diff(app2.calib$DATES) < 7),], 
                          Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # An irregular time step shouldn't work without some effort
  expect_error(expect_warning(residDurbinWatson(reg.model), "Time series is irregular"), "invalid for an irregular time series")
  
  # But if you're willing to sacrifice regularity, you should be able to get a number
  expect_is(residDurbinWatson(reg.model, irregular.timesteps.ok=TRUE, plot=FALSE), "numeric")
  
  # And if it's regular already, it should just work.
  newdata <- transform(simpledata)
  expect_is(residDurbinWatson(reg.model, irregular.timesteps.ok=TRUE, plot=FALSE), "numeric")
  
})

test_that("estimateRho works", {
  library(rloadest)
  # make the dates regular so that we can pretend this dataset makes sense
  simpledata <- app2.calib[-which(diff(app2.calib$DATES) < 7),]
  simpledata <- transform(simpledata, DATES=seq(DATES[1], DATES[length(DATES)], length.out=length(DATES)))
  simpledata <- transform(simpledata, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  reg.model <- loadReg2(loadReg(
    Atrazine ~ Period*center(log(FLOW)), 
    data = simpledata,
    flow = "FLOW", dates = "DATES", conc.units="mg/L"))
  
  # Call estimateRho
  rho.out <- estimateRho(load.model=reg.model, flux.or.conc="flux", abs.or.rel.resids="absolute", newdata=NULL, plot.acf=TRUE, irr=TRUE)
  
  # Output should be a function and a fitted Arima model
  expect_is(rho.out$rho, "numeric")
  expect_is(rho.out$time.step, "difftime")
  expect_is(rho.out$rho.fun, "function") # a function that takes a date and a vector of dates and returns a vector of correlations
  expect_is(rho.out$cormat.fun, "function") # a function that takes a vector of dates and returns a matrix of correlations
  expect_is(rho.out$arima.model, "Arima") # the arima fit
  
  # Check the function for the regular time series to which it was fitted
  #plot(rho.out$rho.fun(simpledata$DATES[3], simpledata$DATES), x=simpledata$DATES, type="b", col="blue")
  # points(rho.out$rho.fun(simpledata$DATES[8], simpledata$DATES), x=simpledata$DATES, type="b", col="green")
  # points(rho.out$rho.fun(simpledata$DATES[14], simpledata$DATES), x=simpledata$DATES, type="b", col="gold")
  # points(rho.out$rho.fun(simpledata$DATES[21], simpledata$DATES), x=simpledata$DATES, type="b", col="red")
  expect_manual_OK("correlations at dates #3, 8, 14, & 21 are OK for regular time series", "Look at the plot.")
  
  # Check the function for an irregular time series - correlation should be a
  # function of the distance in time rather than in the number of rows
  # separating two observations
  simpledata <- app2.calib[-which(diff(app2.calib$DATES) < 7),]
  #plot(rho.out$rho.fun(simpledata$DATES[3], simpledata$DATES), x=simpledata$DATES, type="b", col="blue", ylab="Cor from Dates")
  # points(rho.out$rho.fun(simpledata$DATES[8], simpledata$DATES), x=simpledata$DATES, type="b", col="green")
  # points(rho.out$rho.fun(simpledata$DATES[14], simpledata$DATES), x=simpledata$DATES, type="b", col="gold")
  # points(rho.out$rho.fun(simpledata$DATES[21], simpledata$DATES), x=simpledata$DATES, type="b", col="red")
  expect_manual_OK("correlations at dates #3, 8, 14, & 21 are OK for irregular time series", "Look at the plot.")
  
  # Check that other date formats are also fine
  simpledata <- app2.calib[-which(diff(app2.calib$DATES) < 7),] # these DATES are in Date format
  #plot(rho.out$rho.fun(simpledata$DATES[3], simpledata$DATES), x=simpledata$DATES, type="b", col="blue", ylab="Cor from varied date formats")
  # points(rho.out$rho.fun(as.POSIXlt(simpledata$DATES[8]), as.POSIXlt(simpledata$DATES)), x=simpledata$DATES, type="b", col="green")
  # library(chron)
  # points(rho.out$rho.fun(as.chron(simpledata$DATES[14]), as.chron(simpledata$DATES)), x=simpledata$DATES, type="b", col="gold")
  # points(rho.out$rho.fun(as.POSIXct(simpledata$DATES[21]), as.POSIXct(simpledata$DATES)), x=simpledata$DATES, type="b", col="red")
  # expect_manual_OK("exact same plot when built from varying date formats", "Look at the plot.")
})
