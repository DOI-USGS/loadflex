tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("loadModel objects can be created and validated", {
  # The simplest case: an empty loadModel
  expect_is(new("loadModel"), "loadModel")
  
  # The new() function creates an invalid object because data must be user-supplied
  expect_error(validObject(new("loadModel")))
  
  # A valid loadModel may be constructed using new() if you know what you're doing
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10+rnorm(10,0,0.2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mylm <- lm(conc ~ discharge, data=mydat)
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  myff <- function(training.data) { lm(conc ~ discharge, data=training.data) }
  myloadModel <- new("loadModel", fit=mylm, pred.format="conc", metadata=mymd, fitting.function=myff)
  expect_is(myloadModel, "loadModel")
  expect_true(validObject(myloadModel))
  
  # A valid loadModel is even better created with loadModel()
  myloadModel <- loadModel(inner.fit.function=myff, pred.format="conc", data=mydat, metadata=mymd)
  expect_is(myloadModel, "loadModel")
  expect_true(validObject(myloadModel))
})

test_that("loadModel models implement the loadModel interface", {
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10+rnorm(10,0,0.2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  myff <- function(training.data) { lm(conc ~ discharge, data=training.data) }
  myloadModel <- loadModel(inner.fit.function=myff, pred.format="conc", data=mydat, metadata=mymd)
  
  # Use the standard validation function to test this
  expect_true(validLoadModelInterface(myloadModel))
})

test_that("loadModel models make predictions in the expected units", {
  data(eg_metadata)
  mydat <- data.frame(conc=c(5,4,2,6,9,8,9,7,4,3),discharge=10+rnorm(10,0,0.2),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- updateMetadata(eg_metadata, constituent="conc", flow="discharge", dates="datetime")
  myff <- function(training.data) { lm(log(conc)-7 ~ discharge, data=training.data) }
  myloadModel <- loadModel(inner.fit.function=myff, pred.format="conc", data=mydat, metadata=mymd, retrans.function=function(preds) { exp(preds + 7) })

  expect_equal(get_units(predictSolute(myloadModel, flux.or.conc="conc", newdata=mydat, attach.units=TRUE)), "mg L^-1")
  expect_equal(get_units(predictSolute(myloadModel, flux.or.conc="conc", newdata=mydat, attach.units=FALSE)), NA)
  
  expect_equal(get_units(predictSolute(myloadModel, flux.or.conc="flux", newdata=mydat, attach.units=TRUE)), "kg d^-1")
  expect_equal(get_units(predictSolute(myloadModel, flux.or.conc="flux", newdata=mydat, attach.units=FALSE)), NA)
})

test_that("Tranformation and retransformation make sense", {
  mydat <- data.frame(conc=c(5:14),discharge=5:14*2+rnorm(10,0,0.5),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  
  # Show that lm() will transform the y variable for you, but you have to convert it back to linear space yourself (with exp, here)
  expect_equal(predict(lm(conc ~ discharge, data=mydat)), exp(predict(lm(log(conc) ~ discharge, data=mydat))), tolerance=1)
  
  # Here's how to use the retrans.function for a logged y variable (with bonus demo that you can scale(discharge))
  mylM <- loadModel(inner.fit.function=function(training.data) { lm(log(conc) ~ scale(discharge), data=training.data) }, 
                    pred.format="conc", data=mydat, metadata=mymd, retrans.function=function(preds) { exp(preds) })
  expect_equivalent(predictSolute(mylM, flux.or.conc="conc"), exp(predict(lm(log(conc) ~ discharge, data=mydat))))
  
  # This works for all sorts of nonlinear transformations, including sqrt, ^3,
  # etc. - again, you can retransform using the one-paramter version of 
  # retrans.function
  mylM <- loadModel(inner.fit.function=function(training.data) { lm(sqrt(conc) ~ discharge, data=training.data) }, 
                    pred.format="conc", data=mydat, metadata=mymd, retrans.function=function(preds) { preds^2 })
  expect_equivalent(predictSolute(mylM, flux.or.conc="conc"), (predict(lm(sqrt(conc) ~ discharge, data=mydat)))^2)
  
  # For even more complicated transformations, such as scaling the LHS, you can 
  # manually rescale using the two-parameter version of retrans.function
  mylM <- loadModel(inner.fit.function=function(training.data) { lm(scale(log(conc)) ~ scale(log(discharge)) + 0, data=training.data) }, 
                    pred.format="conc", data=mydat, metadata=mymd, 
                    retrans.function=function(preds, fit) { 
                      rescalars <- attributes(fit$model[["scale(log(conc))"]])[c("scaled:center","scaled:scale")]
                      exp((preds * rescalars$"scaled:scale") + rescalars$"scaled:center")
                    })
  expect_equivalent(predictSolute(mylM, flux.or.conc="conc"), exp(predict(lm(log(conc) ~ scale(log(discharge)), data=mydat))))
  
})

test_that("loadModels fail gracefully when asked for prediction and confidence intervals", {

  # Example data and loadModel
  mydat <- data.frame(conc=c(5:14),discharge=5:14*2+rnorm(10,0,0.5),datetime=strptime(paste0("2000-05-",1:10),format="%Y-%m-%d"))
  mymd <- metadata(
    constituent="conc", flow="discharge", load.rate="", dates="datetime",
    conc.units="mg/L", flow.units="cfs", load.units="kg", load.rate.units="kg/day",
    station="", custom=NULL)
  mylM <- loadModel(inner.fit.function=function(training.data) { lm(log(conc) ~ discharge, data=training.data) }, 
                    pred.format="conc", data=mydat, metadata=mymd, retrans.function=function(preds) { exp(preds) })
  
  # Predictions can be made in either format
  expect_is(predictSolute(mylM, "flux"), "numeric")
  expect_is(predictSolute(mylM, "conc"), "numeric")
  
  # However, uncertainty intervals and parameters are unavailable and announced as such
  expect_error(predictSolute(mylM, "conc", interval="confidence"), "not implemented")
  expect_error(predictSolute(mylM, "conc", interval="prediction"), "not implemented")
  expect_error(predictSolute(mylM, "conc", se.fit=TRUE), "not implemented")
  expect_error(predictSolute(mylM, "conc", se.pred=TRUE), "not implemented")
  
})