# loadReg2

# We initially extended the current loadReg class to implement
# loadModelInterface, but that implementation was risky - getOriginalData could
# work correctly one time and not the next, for example.

#### loadReg2 class ####

#' A load model class specific to loadReg objects produced by the USGS rloadest
#' package.
#' 
#' This class is closely related to loadModel except that the model slot is more
#' strictly defined, and the associated methods are a little different.
#' 
#' @include loadModel.R
#' @include loadReg.R
#' @rdname loadReg2-class
#' @name loadReg2-class
#' @importFrom methods setClass
#' @exportClass loadReg2
#' @family load.model.classes
setClass(
  "loadReg2", 
  
  # S3 objects can't be listed as types here, so we'll enforce loadReg types in validity() below.
  #   slots=c( 
  #     #fit="loadReg"),
  
  contains="loadModel",
  
  # Validity of parent objects is checked first, automatically
  validity=function(object) {
    errorstrs <- character()
    
    if(!is(object@fit, "loadReg")) {
      errorstrs <- c(errorstrs, "object@fit should be a loadReg object")
    }
    
    md <- object@metadata
    if(md@load.rate.units != paste0(md@load.units, " d^-1")) {
      errorstrs <- c(errorstrs, "for loadReg2 models, object@metadata@load.rate.units must equal object@metadata@load.rate / day")
    }
    
    # Return
    if(length(errorstrs) == 0) {
      TRUE
    } else {
      errorstrs
    }
  }    
)

#' Create a fitted loadReg2 object.
#' 
#' Generates a new model of class loadReg2 (\code{\link{loadReg2-class}}). loadReg2s are wrappers for loadReg 
#' objects produced by the USGS \pkg{rloadest} package. \code{loadReg2}s can implement the 
#' \code{\link{loadModelInterface}} more reliably than is possible for a \code{loadReg} object.
#' 
#' @param load.reg An unevaluated call to \code{\link[rloadest]{loadReg}}. This 
#'   call will be parsed and evaluated within \code{loadReg2} to create a fully 
#'   functional load model for use within \pkg{loadflex}.
#' @param pred.format character is the model for flux or concentration can 
#'   be "flux" or "Conc"
#' @param store One or more character strings specifying which information to 
#'   write within the model. Options are 'data': the original fitting data; 
#'   'fitting.function': a fitting function that can produce a new loadComp 
#'   object from new data
#' @param ... Other arguments passed to this model. 
#' @return A fitted loadReg2 model.
#' 
#' @importFrom rloadest loadReg
#' @export
#' @family load.model.inits
loadReg2 <- function(load.reg,
                     pred.format=c("flux","conc"),
                     store=c("data","fitting.function"),
                     ...) {
  
  # Validate arguments
  pred.format <- match.arg.loadflex(pred.format)
  store <- match.arg.loadflex(store, choices=c("data","fitting.function"), several.ok=TRUE)
  
  # Require that load.reg is a call to loadReg (and not an already-generated
  # loadReg) to ensure that we can extract the right information from it.
  loadReg_call <- substitute(load.reg)
  if(is(loadReg_call, "name")) { 
    # The refitting function [only] will pass the call as a symbol (name) 
    # containing language that is itself a call to loadReg. A clever user could 
    # probably trick this test into thinking they're inputting a refit...that's
    # on them to avoid.
    loadReg_call <- load.reg
    is_refit <- TRUE
  } else {
    # If loadReg_call isn't a symbol, then we're fitting this model for the
    # first time (at least, it's not an official refitting)
    is_refit <- FALSE
  }
  # By now, loadReg_call should always be a call to loadReg(). Check that it is.
  call_ok <- FALSE
  if(is(loadReg_call, "call")) {
    if(loadReg_call[[1]] == "loadReg") {
      call_ok <- TRUE
      # one option for the future is to replace the function name with the 
      # namespace::function version, as in "loadReg_call[[1]] <- 
      # quote(rloadest::loadReg)". someday this may be sufficient to evaluate an
      # rloadest model, but we're not there yet. For one thing, loadReg calls 
      # may include additional loadReg functions such as center(). For another, 
      # rloadest:::loadestTadj appears to call dectime without declaring imports
      # smwrBase, which means you need to have rloadest actually loaded so that
      # its dependencies (including smwrBase) are also loaded.
    }
  }
  if(!call_ok) {
    stop("load.reg must be a call of the form loadReg(...)")
  }
  
  # Require that the rloadest namespace has been loaded by the user. Can't do 
  # the package loading here because we would get the WARNING "'library' or 
  # 'require' call to 'rloadest' in package code." during R CMD check. Can't 
  # import the relevant rloadest functions into this function because they're 
  # not openly named anywhere in this function code, which means R CMD check 
  # would worry that we're importing things without using them. So instead we'll
  # require the user to do the loading for us.
  checkRloadestStatus()
  
  # Fit or refit the model
  if(is_refit) {
    # Refitting occurs in the context (environment) of refit.envir, which is an 
    # argument that we've left off the list of documented arguments (it's in
    # ...) because normal users shouldn't be aware of it or using it explicitly.
    if(!("refit.envir" %in% names(list(...)))) {
      stop("Expecting a refit.envir when load.reg is a symbol")
    }
    refit_envir <- list(...)[["refit.envir"]]
    # Now refit in the refit_envir. enclos works because refit_envir is actually a list.
    load.reg <- eval(loadReg_call, envir=refit_envir, enclos=parent.frame())
  } else {
    # if first time, fit in parent environment as a straight loadReg call would
    load.reg <- eval(loadReg_call, envir=parent.frame()) 
  }
  
  # Get the metadata, which we'll need to do the following data checks
  meta <- getMetadata(load.reg)
  
  # Parse the loadReg call, attaching argument names as needed. This will be
  # useful both for the refitting function and for saving the original data.
  if("fitting.function" %in% store | "data" %in% store) {
    loadReg_call <- match.call(definition=loadReg, call=loadReg_call)
  }
  
  # Pull the data into local memory for error checking. Look for easy problems -
  # namely, 0's or negative numbers in the discharge or concentration
  # measurements, which will have been logged without noting problems in
  # loadReg().
  load.model.data <- eval(loadReg_call[["data"]], envir=parent.frame())
  if(any(getCol(metadata=meta, data=load.model.data, "flow", attach.units=FALSE) <= 0, na.rm=TRUE)) {
    warning("Some discharge values are <= 0; this can cause flawed loadReg model fits without further warning")
  }
  if(any(getCol(metadata=meta, data=load.model.data, "conc", attach.units=FALSE) <= 0, na.rm=TRUE)) {
    warning("Some concentration values are <= 0; this can cause flawed loadReg model fit without further warning")
  }
  
  ## REFITTING FUNCTION ##
  if("fitting.function" %in% store) {
    # Create an environment (or identify the existing one, if is_refit) where
    # each argument to loadReg() is saved as an object.
    if(!is_refit) {
      # Start creating the loadReg piece of the refitting function. We will 
      # eventually keep only the reference to loadReg and the formula, so
      # everything else is simply a placeholder.
      loadReg_recall <- loadReg_call
      
      # In the refitting function, the loadReg() call should refer to
      # training.data instead of the original fitting data.
      loadReg_recall[["data"]] <- as.symbol("training.data")
      
      # All other arguments in loadReg_recall should be the same as in the 
      # original call. But if those arguments contain any symbols, then the 
      # value of those symbols might change, and the behavior of 
      # refitting_function could be quite unpredictable. To avoid this, we will
      # copy the value of each argument into a new, safe environment, and we
      # will change the references in loadReg_recall to refer to those saved
      # copies instead of the originals.
      
      # Here's the new, safe environment.
      refit_envir <- new.env(parent=parent.frame(n=2))
      
      # Change all arguments except "formula" and "data" to point into refit_envir
      save_these <- !(names(loadReg_call) %in% c("", "formula", "data"))
      for(argname in names(loadReg_call)[save_these]) {
        savename <- paste0("saved.", argname)
        # Copy the argument value into refit_envir under a new name (savename)
        assign(savename, eval(loadReg_call[[argname]], envir=parent.frame()), envir=refit_envir)
        # Change the calling function to refer to this saved copy
        loadReg_recall[[argname]] <- as.symbol(savename)
      }
      
      # For all that talk about environments, it's actually easier for
      # refit_envir to be a list; this will permit us to use eval() with an
      # enclos argument that actually gets used instead of ignored.
      refit_envir <- as.list(refit_envir)
      
      
    } else {
      # If the refit.envir argument was supplied, then (1) we have already
      # loaded refit_envir above, and (2) we will assume loadReg_call already
      # contains references to objects within refit_envir - i.e., is already the
      # call we want to use for refitting.
      loadReg_recall <- loadReg_call
    }
    
    # The full refitting function wraps loadReg_recall within a loadReg2 call. 
    # refitting_function is a closure that will remember the current values of
    # loadReg_recall, pred.format, store, and refit_envir exactly as they
    # are now.
    load.model.fitting.function <- function(training.data, store=c()) {
      loadReg2(loadReg_recall, pred.format=pred.format, store=store, refit.envir=refit_envir)
    }
  }
  
  # Create and return the model
  load.model <- new(
    "loadReg2", 
    fit=load.reg,   
    pred.format=pred.format,
    metadata=meta,
    data=if("data" %in% store) load.model.data else NULL,
    fitting.function=if("fitting.function" %in% store) load.model.fitting.function else NULL)
}



#' Make flux or concentration predictions from a loadReg2 model.
#' 
#' Makes instantaneous predictions (at the temporal resolution of 
#' \code{newdata}) from a fitted \code{\link{loadReg2}} model. See 
#' \code{\link{predictSolute}} for details.
#' 
#' @inheritParams predictSolute
#' @param load.model A loadReg2 object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names
#'   should match those given in the \code{loadReg2} metadata. If \code{newdata}
#'   is not supplied, the original fitting data will be used.
#' @return A vector of data.frame of predictions, as for the generic 
#'   \code{\link{predictSolute}}.
#' @importFrom rloadest predConc predLoad
#' @export
#' @family predictSolute
predictSolute.loadReg2 <- function(
  load.model, flux.or.conc=c("flux","conc"), newdata, 
  interval=c("none","confidence","prediction"), level=0.95, 
  se.fit=FALSE, se.pred=FALSE, date=FALSE, attach.units=FALSE, ...) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  interval <- match.arg.loadflex(interval)
  
  # Validate rloadest status
  checkRloadestStatus()
  
  # Extract the metadata in object into our standard metadata format, and modify
  # it if load.units has been passed in separately. We'll use the metadata just
  # a couple of times in this function.
  metadata <- getMetadata(load.model)
  if(length(list(...)) > 0) {
    args <- list(...)
    argnames <- names(args)
    accepted_argnames <- c("load.units", "seopt", "print") #"conf.int"
    if(is.null(argnames) | any(!(argnames %in% accepted_argnames))) {
      stop("names of args in ... should be in this list: ", paste0(accepted_argnames, collapse=", "))
    }
    if("load.units" %in% argnames) {
      metadata <- updateMetadata(metadata, load.units=args$load.units)
      metadata <- updateMetadata(metadata, load.rate.units=paste0(args$load.units, "/day"))
    }
  }
  
  # If newdata==NULL, we should return the predictions from the fitting data.
  if(missing(newdata)) {
    newdata <- getFittingData(load.model)
  }
  
  # If time.step != "instantaneous", input data are aggregated to time.step by 
  # rloadest::predLoad()/predConc() before being used for prediction. This is 
  # logical - predictor data should be at the same level of aggregation as the 
  # data with which the model was calibrated. But predict.loadReg() is supposed 
  # to act like other predict() methods, and so we'll emit a message if there's 
  # any aggregation, to make it clearer that something non-standard is
  # happening.
  if(load.model@fit$time.step != "instantaneous") {
    if(load.model@fit$time.step == "day" & class(getCol(metadata, newdata, "date"))[1] == "Date") {
      # do nothing in this case because there won't be aggregation
    } else {
      # but if it's not the day-day case, tell the user what's going to happen
      message("Predictor data will be aggregated to time.step='",load.model@fit$time.step,
              "' before applying model. To override, set time.step='instantaneous'")
    }
    # NB: Whenever time.step == "instantaneous", loadReg/loadConc requires that 
    # dates be in POSIXct format. We'll let those functions deal with any user
    # errors on this point.
  }
  
  # Get around data size constraints of predLoad and predConc by splitting the 
  # prediction into smaller chunks as necessary. chunk.size is the maximum
  # number of values per chunk; rloadest can handle at most 176000 values by
  # default. nchunks is the number of chunks required.
  chunk.size <- 176000
  nchunks <- ceiling(nrow(newdata) / chunk.size)
  newdata <- lapply(1:nchunks, function(i) {
    newdata[((i-1)*chunk.size + 1):min(i*chunk.size, nrow(newdata)),]
  })
  
  # Now do the prediction for each chunk and then reassemble the full set of
  # predictions
  preds_raw <- lapply(newdata, function(datachunk) {
    switch(
      flux.or.conc,
      "flux"=predLoad(
        # ... may contain load.units, seopt, print
        fit=load.model@fit, newdata=datachunk, by="unit", allow.incomplete=FALSE, conf.int=level, ...), 
      "conc"=predConc(
        fit=load.model@fit, newdata=datachunk, by="unit", allow.incomplete=FALSE, conf.int=level) 
    )
  })
  preds_raw <- do.call(rbind, preds_raw)
  
  # Format predictions; add intervals if requested
  preds_col <- .sentenceCase(flux.or.conc)
  if(interval == "none") {
    preds <- formatPreds(preds_raw[[preds_col]], 
                         from.format=flux.or.conc, to.format=flux.or.conc, 
                         newdata=NULL, metadata=metadata, attach.units=attach.units)
  } else if(interval == "confidence") {
    stop("confidence intervals not implemented for loadReg2 models")
  } else if(interval=="prediction") {
    # From the predLoad/predConc documentation: "The term confidence interval is
    # used here as in the original documentation for LOADEST, but the values
    # that are reported are the prediction intervals, computed from the SEP."
    pred.int.num <- as.character(round(level*100, 0))
    preds <- list(fit=preds_raw[[preds_col]], lwr=preds_raw[[paste0("L",pred.int.num)]], upr=preds_raw[[paste0("U",pred.int.num)]])
    preds <- as.data.frame(lapply(preds, function(pr) {
      formatPreds(pr, from.format=flux.or.conc, to.format=flux.or.conc, newdata=NULL, metadata=metadata, attach.units=attach.units)
    }))
  }
  
  # If se.fit==TRUE or se.pred==TRUE, format the output to approximately
  # parallel the output of other predict functions: a list of vectors with
  # SE-related information
  if(se.fit | se.pred) {
    if(!is.data.frame(preds)) {
      preds <- data.frame(fit=preds)
    }
    if(se.fit) {
      preds$se.fit <- preds_raw[["Std.Err"]]
    }
    if(se.pred) {
      preds$se.pred <- preds_raw[["SEP"]]
    }
  }
  
  # Add dates if requested
  if(date) {
    if(!is.data.frame(preds)) {
      preds <- data.frame(fit=preds)
    }
    # prepend the date column, reassembling it from the chunks in newdata
    preds <- data.frame(
      date=do.call(c, lapply(newdata, function(datachunk) { getCol(metadata, datachunk, "date") })), preds)
  }
  
  return(preds)
}




#' Produce a set of predictions that reflect the coefficient uncertainty and
#' possibly also natural variation.
#' 
#' This function resamples the coefficients from their joint distribution, then 
#' makes predictions whose individual errors are sampled from a time series with
#' the same first-order autocorrelation as the original series of errors.
#' 
#' @inheritParams simulateSolute
#' @param load.model A loadReg2 object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names 
#'   should match those given in the \code{loadReg2} metadata. If 
#'   \code{newdata} is not supplied, the original fitting data will be used.
#' @return A vector of predictions that are distributed according to the 
#'   uncertainty of the coefficients and the estimated natural variability + 
#'   measurement error.
#' @export
#' @family simulateSolute
simulateSolute.loadReg2 <- function(load.model, flux.or.conc=c("flux","conc"), newdata, 
                                    method=c("parametric", "non-parametric"), from.interval=c("confidence", "prediction"), rho, ...) {
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  from.interval <- match.arg.loadflex(from.interval, c("confidence","prediction"))
  method <- match.arg.loadflex(method, c("parametric", "non-parametric"))
  if(missing(newdata)) newdata <- getFittingData(load.model) # need to do this for predictSolute call in case non-parametric method overwrites load.model@data
  checkRloadestStatus()
  
  # Generate bootstrap model
  if(method=="parametric") {
    # Resample the coefficients and put them into the load.model@fit
    load.model@fit <- resampleCoefficients.loadReg(load.model@fit, flux.or.conc)
  } else if(method=="non-parametric") {
    # Refit the model to new (resampled) data - this probably takes longer but has nice statistical properties
    df <- getFittingData(load.model)
    load.model <- getFittingFunction(load.model)(
      training.data=df[sample.int(nrow(df), replace=TRUE), ], 
      store=c())
  }
  
  # Make predictions for the new data
  fitting.preds <- predictSolute(load.model, flux.or.conc=flux.or.conc, newdata=newdata, interval="none")
  
  ## Resample Errors ##
  
  # EXPERIMENTAL: To keep the autocorrelation that's necessary for loadComp to
  # make sense (even though it's assumed not to exist in the calibration data),
  # either (1) estimate autocorrelation from the calibration data, or (2) accept
  # an estimate of autocorrelation from the user. Then use that autocorrelation
  # and the residual standard deviation to add noise to the predictions with an
  # autocorrelation structure based on that of the residuals (or supplied by the
  # caller)
  if(from.interval == "prediction") {
    
    if(missing(rho)) {
      # NOT FINISHED: The abs.or.rel and use.log arguments here are specific to most but not all models - make more robust
      rho.out <- estimateRho(load.model, flux.or.conc=flux.or.conc, abs.or.rel.resids="absolute", use.log=TRUE, 
                             newdata=newdata, irregular.timesteps.ok=TRUE, plot.acf=FALSE)
      rho <- rho.out$rho # implies that the time.step for newata is the one we want
    }
    # Simulate the residuals so that they have the autocorrelation structure 
    # that was estimated from the original residuals or supplied by the caller.
    # Not sure whether rho takes care of matching the distribution of resampled
    # residuals to the original; it seems not to.
    noise <- as.numeric(arima.sim(model=list(order=c(1,0,0), ar=rho), n=nrow(newdata)))
    # Normalize the noise to have the same sd as the original residuals. Is that reasonable?
    error <- "print.function"
    error("need to calculate s.hat")
    #noise <- noise * s.hat/sd(noise)
    # Add the noise back into the predictions, first converting back to linear space
    fitting.preds <- exp(log(fitting.preds) + noise)
  }
  
  return(fitting.preds)
}
