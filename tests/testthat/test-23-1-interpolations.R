tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

# A helper function to test each type of interpolation
checkInterpPreds <- function(interp.fun) {
  # data for calibration of the interpolation models
  mydat <- data.frame(
    conc=c(1.3,-0.5,0,1.2,-0.1,1.4,-2.3,-0.4,0.7,0.5),
    datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  
  # data for prediction (interpolation)
  mydat2 <- data.frame(datetime=strptime(paste0("2000-05-",rep(1:10, each=24)," ",rep(0:23, 10)),format="%Y-%m-%d %H"))
  
  # Fit an interpolation model & make predictions
  lic <- loadInterp(interp.format="conc", data=mydat, interp.function=interp.fun, metadata=mymd)
  preds <- data.frame(mydat2, conc=predictSolute(lic, "conc", newdata=mydat2))
  
  # Make and plot the predictions and the original data
  #print(ggplot(preds, aes(x=datetime, y=conc)) + 
  #        geom_point(color="pink") + geom_line(color="pink") + 
  #        geom_point(data=mydat, shape=2, size=3, color="blue") + theme_bw())
  
  # Get the tester's OK
  expect_manual_OK(paste0(as.character(substitute(interp.fun))[1],"s (pink) look good with respect to observations (blue)"))
}

test_that("linearInterpolations look like they should", {
  checkInterpPreds(linearInterpolation)
})
test_that("triangularInterpolations look like they should", {
  checkInterpPreds(triangularInterpolation)
  checkInterpPreds(getTriangularInterpolation(0))
  checkInterpPreds(getTriangularInterpolation(0.75))
})
test_that("rectangularInterpolations look like they should", {
  checkInterpPreds(rectangularInterpolation)
})
test_that("splineInterpolations look like they should", {
  checkInterpPreds(splineInterpolation)
})
test_that("smoothSplineInterpolations look like they should", {
  checkInterpPreds(smoothSplineInterpolation)
  checkInterpPreds(getSmoothSplineInterpolation(df=3))
  checkInterpPreds(getSmoothSplineInterpolation(df=8))
  checkInterpPreds(getSmoothSplineInterpolation(spar=0.6))
  checkInterpPreds(getSmoothSplineInterpolation(spar=0.15))
})
test_that("distanceWeightedInterpolations look like they should", {
  checkInterpPreds(distanceWeightedInterpolation)
  checkInterpPreds(getDistanceWeightedInterpolation(inv.dist.fun=function(a,b) { 1 / abs(a-b) })) # neat!
  checkInterpPreds(getDistanceWeightedInterpolation(inv.dist.fun=function(a,b) { exp(-abs(a-b)/20000) })) # quite sensitive to the 20000
  checkInterpPreds(getDistanceWeightedInterpolation(inv.dist.fun=function(a,b) { 1 / ((a-b)^4) }))
})



