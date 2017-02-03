#' Functions implemented by any \code{loadflex}-compatible load model.
#' 
#' Solute load models in the \code{loadflex} package, such as \code{loadModel}, 
#' \code{loadReg2}, and \code{loadComp}, all implement a common set of core 
#' functions. These functions are conceptually packaged as the 
#' \code{loadModelInterface} defined here.
#' 
#' @section Functions in the interface: \itemize{ \item 
#'   \code{\link{getMetadata}(load.model) \{ return(metadata) \}} \item 
#'   \code{\link{getFittingData}(load.model) \{ return(data.frame) \}} \item 
#'   \code{\link{getFittingFunction}(load.model) \{ return(function) \}} \item 
#'   \code{\link{predictSolute}(load.model, flux.or.conc, newdata, interval, 
#'   level, se.fit, se.pred, attach.units, ...) \{ return(numeric vector or 
#'   data.frame) \}} }
#'   
#' @section Defining new load models: Users may define additional custom load
#'   models for use with \code{loadflex} as long as those models, too, implement
#'   the loadModelInterface. One easy way to implement the interface is to write
#'   the new load model class so that it inherits from the 
#'   \code{\link{loadModel}} class.
#'   
#'   If a new load model class is defined, the user may confirm that the new
#'   class implements the loadModelInterface by running
#'   \code{\link{validLoadModelInterface}}.
#'   
#' @name loadModelInterface
#' @rdname loadModelInterface
#' @docType data
#' @format A collection of functions which any load model for use with 
#'   \code{loadflex} should implement.
NULL
 

#' Extract metadata from a load model.
#' 
#' A function in the loadModelInterface. Returns a load model's metadata, 
#' encapsulated as a \code{metadata} object.
#' 
#' @param load.model A load model, implementing the loadModelInterface, for
#'   which to return the metadata
#' @return Object of class "metadata" with slots reflecting the metadata for
#'   load.model
#' @export
#' @family loadModelInterface
#' @family getMetadata
getMetadata <- function(load.model) {
  UseMethod("getMetadata")
}


#' Extract the data originally used to fit a load model.
#' 
#' A function in the loadModelInterface.
#' 
#' @param load.model A load model, implementing the loadModelInterface, for
#'   which to return the fitting data
#' @return data.frame containing the original fitting data
#' @export
#' @family loadModelInterface
#' @family getFittingData
getFittingData <- function(load.model) {
  UseMethod("getFittingData")
}


#' Get a function that can be used to refit the load.model with new data.
#' 
#' A function in the loadModelInterface. Takes a load.model and returns a 
#' function to fit a new load.model that is identical in every respect except 
#' its training data and resulting model coefficients or other paramters. The
#' returned function should accept exactly one argument, the training data, and
#' should return an object of the same class as load.model.
#' 
#' @param load.model The model for which to return a new fitting function.
#' @return Object of class "function" which
#' @export
#' @family loadModelInterface
#' @family getFittingFunction
getFittingFunction <- function(load.model) {
  UseMethod("getFittingFunction")
}


#' Retrieve the fitted model, if appropriate, from a loadModel load model
#' 
#' A function in the loadModelInterface. Takes a load.model and returns a 
#' function to fit a new load.model that is identical in every respect except 
#' its training data and resulting model coefficients or other paramters. The
#' returned function should accept exactly one argument, the training data, and
#' should return an object of the same class as load.model.
#' 
#' @param load.model The load model for which to return the inner fitted model.
#' @return Object of class "function" which
#' @export
#' @family loadModelInterface
#' @family getFittedModel
getFittedModel <- function(load.model) {
  UseMethod("getFittedModel")
}

#' Make flux or concentration predictions from a load model.
#' 
#' A function in the loadModelInterface. Uses a load model and a predictor 
#' dataset (which may differ from the original model-fitting dataset) to make 
#' predictions for loads or concentrations at the time points in the new 
#' dataset.
#' 
#' This is the S3 generic predictSolute(), for which specific methods should be 
#' implemented for each load model class (e.g., \code{\link{loadModel}}. Unlike 
#' rloadest::predLoad() and predConc(), and more like most other predict 
#' functions in R, this function makes no attempt to aggregate the results.
#' 
#' @param load.model A load model object, typically inheriting from loadModel 
#'   and always implementing the loadModelInterface.
#' @param flux.or.conc indicate if the solute is measured as a flux or 
#'   concentration.
#' @param newdata An optional data.frame of predictor observations. The column 
#'   names in this data.frame must match those specified in the load model's 
#'   metadata.
#' @param interval character. One of "none", "confidence" or "prediction".
#'   If "confidence" or "prediction", the interval bounds will be returned in 
#'   columns named "lwr" and "upr". Confidence intervals describe confidence in 
#'   the model prediction for the mean value given a set of predictors, whereas 
#'   prediction bounds describe the expected distribution of observations at 
#'   that prediction point.
#' @param level numeric. Fraction of density distribution to include within confidence or
#'   prediction interval
#' @param se.fit logical. If TRUE, the output data.frame will include a column 
#'   named "se.fit" describing the standard error of the model fit for each row 
#'   of predictors.
#' @param se.pred logical. If TRUE, the output data.frame will include a column 
#'   named "se.pred" describing the standard error of the prediction for each 
#'   row of predictors. The values in the se.pred column will be larger than 
#'   those in the se.fit column, because the se.pred values are standard errors 
#'   of prediction (SEPs) and take into account not only the parameter 
#'   uncertainty associated with the model coefficients (also covered by 
#'   se.fit), but also the random error associated with any given observation 
#'   (the epsilon term in a typical regression model).
#' @param date logical. If TRUE, the output data.frame will include a column 
#'   named "date" containing the dates of the predictions.
#' @param attach.units logical. Should the units be attached to columns in the 
#'   resulting data.frame?
#' @param ... Additional arguments passed to class-specific implementations of 
#'   the \code{predictSolute} generic function.
#' @return If interval=="none" and both se.fit and se.pred are FALSE, a vector 
#'   of predictions. Otherwise, a data.frame with a column called "fit" 
#'   containing the predictions for the solute. Values associated with interval,
#'   se.fit, and se.pred are additional columns with names noted in those 
#'   argument descriptions.
#' @export
#' @family loadModelInterface
#' @family predictSolute
predictSolute <- function(
  load.model, flux.or.conc=c("flux","conc"), newdata, 
  interval=c("none","confidence","prediction"), level=0.95,   
  se.fit=FALSE, se.pred=FALSE, date=FALSE, attach.units=FALSE, 
  log.or.lin=c("linear","log"), ...) {

  UseMethod("predictSolute")
}


#' Simulate solute concentrations based on the model and model uncertainty.
#' 
#' This function is an optional component of the \link{loadModelInterface}. It 
#' is unnecessary for model fitting, assessment, and prediction except when used
#' in conjunction with the composite method (i.e., within a 
#' \code{\link{loadComp}} model).
#' 
#' @param load.model A load model object, typically inheriting from loadModel 
#'   and always implementing the loadModelInterface.
#' @param flux.or.conc indicate if the solute is measured as a flux or 
#'   concentration.
#' @param newdata An optional data.frame of predictor observations. The column 
#'   names in this data.frame must match those specified in the load model's 
#'   metadata.
#' @param method character. The method by which the model should be 
#'   bootstrapped. "non-parametric": resample with replacement from the original
#'   fitting data, refit the model, and make new predictions. "parametric":
#'   resample the model coefficients based on the covariance matrix originally
#'   estimated for those coefficients, then make new predictions.
#' @param from.interval character. The interval type from which to resample 
#'   (simulate) the solute. If "confidence", the regression model coefficients 
#'   are resampled from their multivariate normal distribution and predictions 
#'   are made from the new coefficients. If "prediction", an additional vector 
#'   of noise is added to those "confidence"-based predictions.
#' @param rho An autocorrelation coefficient to assume for the residuals, 
#'   applicable when from.interval=="prediction". If rho is missing and 
#'   interval=="prediction", rho will be estimated from the residuals calculated
#'   from newdata with the fitted (not yet resampled) load.model.
#' @param ... Other arguments passed to inheriting methods
#' @return A vector or data.frame of predictions, as for the generic 
#'   \code{\link{predictSolute}}. The simulated predictions are distributed 
#'   according to the uncertainty of the coefficients (if 
#'   from.interval=="confidence") and also the estimated natural variability + 
#'   measurement error (if from.interval=="prediction").
#' @export
#' @family loadModelInterface
#' @family simulateSolute
simulateSolute <- function(
  load.model, flux.or.conc=c("flux","conc"), newdata, 
  method=c("non-parametric", "parametric"), 
  from.interval=c("confidence", "prediction"), rho, ...) {

  UseMethod("simulateSolute")
}

#' Estimate model uncertainty algorithmically.
#' 
#' This function is an optional component of the \link{loadModelInterface}. It 
#' is unnecessary for model fitting, assessment, and prediction except when used
#' in conjunction with the composite method (i.e., within a 
#' \code{\link{loadComp}} model) or for models such as loadInterps for which the
#' MSE cannot be known without some estimation procedure.
#' 
#' @param load.model A load model object, typically inheriting from loadModel 
#'   and always implementing the loadModelInterface.
#' @param ... Other arguments passed to inheriting methods for estimateMSE
#' @export
#' @family loadModelInterface
#' @family estimateMSE
estimateMSE <- function(load.model, ...) {
  UseMethod("estimateMSE")
}


#' Extract model summary statistics
#' 
#' summarizeModel produces a 1-row data.frame of model metrics. The relevant 
#' metrics vary by model type; only the relevant metrics are reported for each 
#' model.
#' 
#' @param load.model A load model object, typically inheriting from loadModel 
#'   and always implementing the loadModelInterface.
#' @param ... Other arguments passed to model-specific methods
#' @export
#' @family loadModelInterface
#' @family summarizeModel
summarizeModel <- function(load.model, ...) UseMethod("summarizeModel")


#' Test whether a class implements the loadModelInterface
#' 
#' \code{validLoadModelInterface} can be used to test whether the
#' \code{\link{loadModelInterface}} has been successfully implemented for the class of a
#' provided object.
#' 
#' @importFrom methods is
#' @importFrom utils methods
#' @param object an object with a LoadModelInterface
#' @param stop.on.error logical. If the interface is invalid, should the 
#'   function throw an error (TRUE) or quietly return a warning object (FALSE)?
#' @param verbose logical. turn on or off verbose messages. 
#' @return TRUE if interface for given load.model is well defined; otherwise, 
#'   either throws an error (if stop.on.error=TRUE) or returns a vector of 
#'   character strings describing the errors (if stop.on.error=FALSE).
#' @export
#' @family loadModelInterface
validLoadModelInterface <- function(object, stop.on.error=TRUE, verbose=TRUE) {
  cl <- class(object)[[1]]
  msgstrs <- paste0("Required methods for class ", cl, ":")
  errorstrs <- character()
                    
  # Check that each function returns a result of the expected type
  if(!is(getMetadata(object), "metadata")) {
    errorstrs <- c(errorstrs, "getMetadata should return an object of class 'metadata'")
    msgstrs <- paste0(msgstrs, "\n  - getMetadata")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + getMetadata")
  }
  
  fd <- getFittingData(object)
  if(!is(fd, "data.frame")) {
    errorstrs <- c(errorstrs, "getFittingData should return an object of class 'data.frame'")
    msgstrs <- paste0(msgstrs, "\n  - getFittingData")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + getFittingData")
  }
  
  ff <- getFittingFunction(object)
  ffok <- TRUE
  if(!is(ff, "function")) {
    errorstrs <- c(errorstrs, "getFittingFunction should return a function")
    ffok <- FALSE
  } else {
    if(!all(mapply(`==`, formals(ff), alist(training.data=,store=c())))){
      errorstrs <- c(errorstrs, "the fitting function should accept arguments c(training.data=, store=c())")
      ffok <- FALSE
    }
    if(!is(ff(fd), class(object))) {
      errorstrs <- c(errorstrs, "the fitting function should return an object of the same class as this model")
      ffok <- FALSE
    }
  }
  if(!ffok) {
    msgstrs <- paste0(msgstrs, "\n  - getFittingFunction")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + getFittingFunction")
  }
  
  fm <- getFittedModel(object)
  if(is.null(fm)) {
    errorstrs <- c(errorstrs, "getModel should return the internal model fit")
    msgstrs <- paste0(msgstrs, "\n  - getFittedModel")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + getFittedModel")
  }
  
  if(!is(predictSolute(load.model=object, flux.or.conc="flux"), "numeric") |
       !is(predictSolute(load.model=object, flux.or.conc="flux", newdata=fd), "numeric") |
       !is(predictSolute(load.model=object, flux.or.conc="conc", newdata=fd), "numeric")) {
    errorstrs <- c(errorstrs, "predictSolute(object) should return a numeric vector")
    msgstrs <- paste0(msgstrs, "\n  - predictSolute")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + predictSolute")
  }
  
  ms <- summarizeModel(object)
  if(!is(ms, "data.frame") || nrow(ms) != 1) {
    errorstrs <- c(errorstrs, "summarizeModel should return a 1-row data.frame")
    msgstrs <- paste0(msgstrs, "\n  - summarizeModel")
  } else {
    msgstrs <- paste0(msgstrs, "\n  + summarizeModel")
  }
  
  # Optional functions
  msgstrs <- paste0(
    msgstrs,
    "\nOptional methods implemented for class ", cl, ":\n  ",
    if(paste0("simulateSolute.",cl) %in% methods("simulateSolute")) "+" else "-", " simulateSolute", "\n  ",
    if(paste0("estimateMSE.",cl) %in% methods("estimateMSE")) "+" else "-", " estimateMSE"
  )
  
  if(verbose) message(msgstrs)
  
  if(length(errorstrs) == 0) {
    TRUE
  } else {
    if(stop.on.error) {
      stop(errorstrs)
    } else {
      errorstrs
    }
  }
}
