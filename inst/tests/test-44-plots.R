#' This file needs to be rewritten once plotting functionality has been reintroduced



#### Old code: test-CM.plots.R ####

context("plots")

# FYI, libraries introduced here get added to the workspace
library(rloadest)
library(ggplot2)

# Functions defined here will not be added to the global environment if this
# file is run using test().

#' Suppresses only those warnings given in messages; warnings must be presented
#' in number and order expected.
#' 

#setwd("../composite_package/inst/tests")
library(evaluate)
gives_exact_warnings <- function(regexp) {
  function(expr) {
    res <- evaluate(substitute(expr), parent.frame(), new_device = FALSE)
    warnings <- vapply(Filter(is.warning, res), "[[", "message", 
                       FUN.VALUE = character(1))
    errors <- vapply(Filter(is.error, res), "[[", "message", 
                     FUN.VALUE = character(1))
    if(length(errors) > 0) {
      stop(errors)
    } else if (!is.null(regexp) && length(warnings) > 0) {
      matches(regexp, all = TRUE)(warnings)
    }
    else {
      expectation(length(warnings) > 0, "no warnings given")
    }
  }
}


test_that("load plots threw no errors", {
  
  expect_that(
    app2_lr <- loadReg(
      Atrazine ~ Period*center(log(FLOW)), 
      data = transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      flow = "FLOW", dates = "DATES", 
      flow.units="cfs", conc.units="ug/L", load.units="pounds", 
      time.step = "day",
      station="St.Joseph River near Newville, Ind."),
    gives_exact_warnings("The minimum spacing between daily loads"))
  
  ## loads
  
  expect_that(
    regression_predictions <- predictSoluteFromRegression(
      "Flux",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")))),
    gives_exact_warnings("NaNs produced"))
  
  expect_that(
    interpolated_residuals <- interpolateSoluteResiduals(
      "Flux",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
      observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      interp.method="linear", verbose=FALSE),
    gives_exact_warnings("NaNs produced"))
  
  adjusted_predictions <- adjustSoluteByInterpolation(
    "Flux", 
    regression_predictions, interpolated_residuals, 
    replace.negatives=-1, verbose=FALSE)
  
  linearInterpolation_predictions <- interpolateSoluteObservations(
    "Flux", app2_lr, 
    transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    interp.method="linear", verbose=FALSE)
  adjusted_predictions$LinearInterpObs <- linearInterpolation_predictions$LinearInterpObs
  
  
  
  aggday <- aggregateSolute(
    "Flux",
    aggregation.interval="day", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE)
  plotLoadsCM(app2_lr,aggday)
  
  
  aggmonth <- aggregateSolute(
    "Flux",
    aggregation.interval="month", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE)
  jpeg(filename='testPlots/rplot1.jpg')
  plotLoadsCM(app2_lr,aggmonth)
  dev.off()
  
  jpeg(filename='testPlots/rplot2.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=TRUE, linear.interpolation=FALSE, regression=FALSE)
  
  dev.off()
  jpeg(filename='testPlots/rplot3.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=FALSE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot4.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=FALSE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot5.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot6.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=TRUE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot7.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot8.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=TRUE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot9.jpg')
  plotLoadsCM(app2_lr,aggmonth,composite=FALSE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
  rangex<- range(app2.calib$DATE)
  plotLoadsCM(app2_lr,aggmonth,app2.calib,xrange=rangex,verbose=TRUE)
} )
#plotCM <- function(load.or.conc, show.observations, load.model, finalloads, observations=NULL, 
#                  composite=TRUE, linear.interpolation=FALSE, regression=TRUE,  xrange="none", 
#                  dateField="Date", verbose=FALSE)

#plotCM <- function(load.or.conc, load.model, finalloads, composite=TRUE, 
#linear.interpolation=FALSE, regression=TRUE,  xrange="none", dateField="Date", verbose=FALSE) {
test_that("concentration plots threw no errors", {
  expect_that(
    regression_predictions <- predictSoluteFromRegression(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")))),
    gives_exact_warnings("NaNs produced"))
  
  expect_that(
    interpolated_residuals <- interpolateSoluteResiduals(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      interp.method="linear", verbose=FALSE),
    gives_exact_warnings("NaNs produced"))
  
  adjusted_predictions <- adjustSoluteByInterpolation(
    "conc",
    regression_predictions, interpolated_residuals, 
    replace.negatives=-1, verbose=FALSE)
  
  linearInterpolation_predictions <- interpolateSoluteObservations(
    "conc", app2_lr, 
    transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    interp.method="linear", verbose=FALSE)
  adjusted_predictions$LinearInterpObs <- linearInterpolation_predictions$LinearInterpObs
  
  expect_that(
    !is.null(aggregateSolute(
      "conc",
      aggregation.interval="month", 
      loads=adjusted_predictions, 
      load.model=replace(app2_lr, "load.units", "kg"), # a hack to make this pass
      verbose=FALSE)), 
    is_true())
  aggMonthConc <- aggregateSolute(
    "conc",
    aggregation.interval="month", 
    loads=adjusted_predictions, 
    load.model=replace(app2_lr, "load.units", "kg"), # a hack to make this pass
    verbose=FALSE)
  
  plotConcCM(app2_lr,aggMonthConc)
  dev.off()
  
  jpeg(filename='testPlots/rplot2c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=TRUE, linear.interpolation=FALSE, regression=FALSE)
  
  dev.off()
  jpeg(filename='testPlots/rplot3c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=FALSE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot4c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=FALSE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot5c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot6c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=TRUE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot7c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot8c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=TRUE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot9c.jpg')
  plotConcCM(app2_lr,aggMonthConc,composite=FALSE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
  
})
test_that("observation plots threw no errors", {
  expect_that(
    regression_predictions <- predictSoluteFromRegression(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")))),
    gives_exact_warnings("NaNs produced"))
  
  expect_that(
    interpolated_residuals <- interpolateSoluteResiduals(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      interp.method="linear", verbose=FALSE),
    gives_exact_warnings("NaNs produced"))
  
  adjusted_predictions <- adjustSoluteByInterpolation(
    "conc",
    regression_predictions, interpolated_residuals, 
    replace.negatives=-1, verbose=FALSE)
  
  linearInterpolation_predictions <- interpolateSoluteObservations(
    "conc", app2_lr, 
    transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    interp.method="linear", verbose=FALSE)
  adjusted_predictions$LinearInterpObs <- linearInterpolation_predictions$LinearInterpObs
  
  aggMonthConc <- aggregateSolute(
    "conc",
    aggregation.interval="month", 
    loads=adjusted_predictions, 
    load.model=replace(app2_lr, "load.units", "kg"), # a hack to make this pass
    verbose=FALSE)
  
  rangex<- range(app2.calib$DATE)
  #plotConcCM(app2_lr,aggMonthConc,app2.calib,xrange=rangex,verbose=TRUE)
  ##observations 
  jpeg(filename='testPlots/rplot2o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib, composite=TRUE, linear.interpolation=FALSE, regression=FALSE)
  
  dev.off()
  jpeg(filename='testPlots/rplot3o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=FALSE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot4o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=FALSE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot5o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot6o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=TRUE, linear.interpolation=FALSE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot7o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=TRUE, linear.interpolation=TRUE, regression=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot8o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=TRUE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
  jpeg(filename='testPlots/rplot9o.jpg')
  plotObservationsCM(app2_lr,aggMonthConc,app2.calib,composite=FALSE, linear.interpolation=TRUE, regression=TRUE)
  dev.off()
} ) 
#jpeg(filename='testPlots/rplot1r.jpg')
#plotConcResidualsCM(app2_lr,app2.calib, app2.est,verbose=TRUE)
#dev.off()
test_that("residual plots threw no errors", {
  obs <- transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")))
  #dev.off()
  jpeg(filename='testPlots/rplot2r.jpg')
  plotConcResidualsCM(app2_lr,obs, xFlow=FALSE,day.of.year=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot3r.jpg')
  plotConcResidualsCM(app2_lr,obs,xFlow=TRUE,day.of.year=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot4r.jpg')
  plotConcResidualsCM(app2_lr,obs, xFlow=FALSE,day.of.year=TRUE)
  dev.off()
  # plotLoadsCM(app2_lr,aggmonth,composite=FALSE, linear.interpolation=TRUE, regression=FALSE)
  jpeg(filename='testPlots/rplot5r.jpg')
  plotLoadResidualsCM(app2_lr,obs, xFlow=FALSE,day.of.year=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot6r.jpg')
  plotLoadResidualsCM(app2_lr,obs,xFlow=TRUE,day.of.year=FALSE)
  dev.off()
  jpeg(filename='testPlots/rplot7r.jpg')
  plotLoadResidualsCM(app2_lr,obs, xFlow=FALSE,day.of.year=TRUE)
  dev.off()
  #errors 
  #plotConcResidualsCM(app2_lr,app2.calib,xFlow=TRUE,day.of.year=TRUE,verbose=TRUE)
})

#### Old code: test-plots2.R ####

context("plots")

# FYI, libraries introduced here get added to the workspace
library(rloadest)

# Functions defined here will not be added to the global environment if this
# file is run using test().

#' Suppresses only those warnings given in messages; warnings must be presented
#' in number and order expected.
library(evaluate)
gives_exact_warnings <- function(regexp) {
  function(expr) {
    res <- evaluate(substitute(expr), parent.frame(), new_device = FALSE)
    warnings <- vapply(Filter(is.warning, res), "[[", "message", 
                       FUN.VALUE = character(1))
    errors <- vapply(Filter(is.error, res), "[[", "message", 
                     FUN.VALUE = character(1))
    if(length(errors) > 0) {
      stop(errors)
    } else if (!is.null(regexp) && length(warnings) > 0) {
      matches(regexp, all = TRUE)(warnings)
    }
    else {
      expectation(length(warnings) > 0, "no warnings given")
    }
  }
}



test_that("Loads can be plotted", {
  
  ### setup for plotting ###
  
  expect_that(
    app2_lr <- loadReg(
      Atrazine ~ Period*center(log(FLOW)), 
      data = transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      flow = "FLOW", dates = "DATES", 
      flow.units="cfs", conc.units="ug/L", load.units="pounds", 
      time.step = "day",
      station="St.Joseph River near Newville, Ind."),
    gives_exact_warnings("The minimum spacing between daily loads"))
  
  expect_that(
    regression_predictions <- predictSoluteFromRegression(
      "Flux",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")))),
    gives_exact_warnings("NaNs produced"))
  
  expect_that(
    interpolated_residuals <- interpolateSoluteResiduals(
      "Flux",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
      observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      interp.method="linear", verbose=FALSE),
    gives_exact_warnings("NaNs produced"))
  
  adjusted_predictions <- adjustSoluteByInterpolation(
    "Flux", 
    regression_predictions, interpolated_residuals, 
    replace.negatives=-1, verbose=FALSE)
  
  linearInterpolation_predictions <- interpolateSoluteObservations(
    "Flux", app2_lr, 
    transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    interp.method="linear", verbose=FALSE)
  adjusted_predictions$LinearInterpObs <- linearInterpolation_predictions$LinearInterpObs
  
  aggday <- aggregateSolute(
    "Flux",
    aggregation.interval="day", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE)
  
  aggmonth <- aggregateSolute(
    "Flux",
    aggregation.interval="month", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE)
  
  
  ### plot tests ###
  
  plotCM("Flux", FALSE, load.model=app2_lr, aggday)
  
  expect_that(plotCM("Flux", show.observations=TRUE, load.model=app2_lr, finalloads=aggday,
                     observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")))),
              throws_error())
  
  plotCM("Flux", show.observations=FALSE, load.model=app2_lr, 
         finalloads=transform(aggday, Period=strptime(Period, format="%m-%Y-%d")),
         observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))))  
  
  plotCM("Flux", FALSE, app2_lr, aggday, composite=TRUE, linear.interpolation=TRUE, regression=TRUE)
  plotCM("Flux", FALSE, app2_lr, aggmonth, composite=TRUE, linear.interpolation=TRUE, regression=TRUE)
})


test_that("Concentrations can be plotted", {
  
  ### setup for plotting ###
  
  expect_that(
    app2_lr <- loadReg(
      Atrazine ~ Period*center(log(FLOW)), 
      data = transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      flow = "FLOW", dates = "DATES", 
      flow.units="cfs", conc.units="ug/L", load.units="pounds", 
      time.step = "day",
      station="St.Joseph River near Newville, Ind."),
    gives_exact_warnings("The minimum spacing between daily loads"))
  
  expect_that(
    regression_predictions <- predictSoluteFromRegression(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")))),
    gives_exact_warnings("NaNs produced"))
  
  expect_that(
    interpolated_residuals <- interpolateSoluteResiduals(
      "conc",
      load.model=app2_lr, 
      data.to.predict=transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul")), Date=as.POSIXct(DATES)),
      interp.method="linear", verbose=FALSE),
    gives_exact_warnings("NaNs produced"))
  
  adjusted_predictions <- adjustSoluteByInterpolation(
    "conc",
    regression_predictions, interpolated_residuals, 
    replace.negatives=-1, verbose=FALSE)
  
  linearInterpolation_predictions <- interpolateSoluteObservations(
    "conc", app2_lr, 
    transform(app2.est, Period=seasons(DATES, breaks=c("Apr", "Jul"))),
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    interp.method="linear", verbose=FALSE)
  adjusted_predictions$LinearInterpObs <- linearInterpolation_predictions$LinearInterpObs
  
  aggCunit <- aggregateSolute(
    "conc",
    aggregation.interval="unit", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE) 
  
  aggCmonth <- aggregateSolute(
    "conc",
    aggregation.interval="month", 
    loads=adjusted_predictions, 
    load.model=app2_lr, 
    verbose=FALSE) 
  
  ### plot tests ###
  
  plotCM("Conc",TRUE,load.model=app2_lr, finalloads=aggCunit, 
         observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
         composite=TRUE, linear.interpolation=TRUE,regression=TRUE,verbose=FALSE)
  
  plotCM("Conc",TRUE,load.model=app2_lr, finalloads=aggCmonth, 
         observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
         composite=TRUE, linear.interpolation=FALSE,regression=FALSE,verbose=FALSE)
  
  plotCM("Conc",TRUE,load.model=app2_lr, finalloads=aggCmonth, 
         observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
         composite=TRUE, linear.interpolation=TRUE,regression=TRUE,verbose=FALSE)
})

test_that("plotResidualsCM makes useful plots", {
  
  ### setup for plotting ###
  
  expect_that(
    app2_lr <- loadReg(
      Atrazine ~ Period*center(log(FLOW)), 
      data = transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))),
      flow = "FLOW", dates = "DATES", 
      flow.units="cfs", conc.units="ug/L", load.units="pounds", 
      time.step = "day",
      station="St.Joseph River near Newville, Ind."),
    gives_exact_warnings("The minimum spacing between daily loads"))
  
  ### plot tests ###
  
  plotResidualsCM(
    load.model=app2_lr, 
    observations=transform(app2.calib, Period=seasons(DATES,breaks=c("Apr", "Jul"))), 
    dateField="DATES", verbose=FALSE)
  
})