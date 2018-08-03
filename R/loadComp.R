#' #### compModel class ####

#' The engine of a loadInterp model.
#' 
#' This model class is designed to nest within loadComp objects. The compModel 
#' class is actually a container for two inner models implementing the loadModel
#' interface: reg.model and resid.model. reg.model will usually be a regression 
#' model, often of type loadReg. resid.model is typically a loadInterp, used to 
#' interpolate among the residuals of the reg.model predictions.
#' 
#' @include loadModel.R
#' @rdname compModel-class
#' @name compModel-class
#' @slot reg.model The regression model - any model that implements the 
#'   loadModelInterface.
#' @slot resid.model The interpolation model - any model that implements the 
#'   loadModelInterface.
#' @slot log.resids Should residuals be computed in log space (TRUE) or 
#'   linear space (FALSE)?
#' @slot abs.or.rel.resids character specifying whether interpolation should be 
#'   by absolute residuals (obs-pred) or relative residuals (obs/pred).
#' @importFrom methods setClass
#' @exportClass compModel
#' @family load.model.fits
setClass(
  "compModel",
  
  slots=c(
    reg.model="ANY",
    resid.model="ANY",
    log.resids="logical",
    abs.or.rel.resids="character"),
  
  prototype=c(
    reg.model=NULL,
    resid.model=NULL,
    log.resids=NA,
    abs.or.rel.resids=NA),
  
  validity=function(object) {
    errorstrs <- character()
    
    if(length(errorstrs) > 0) {
      errorstrs
    } else {
      TRUE
    }
  }
)

#### loadComp class ####

#' A load model class implementing the composite method for flux estimation.
#' 
#' @importFrom methods setClass
#' @exportClass loadComp
#' @family load.model.classes
setClass(
  "loadComp", 
  
  slots=c( 
    fit="compModel",
    MSE="matrix"),
  
  contains="loadModel",
  
  # Validity of parent objects is checked first, automatically
  validity=function(object) {
    errorstrs <- character()
    
    # Return
    if(length(errorstrs) == 0) {
      TRUE
    } else {
      errorstrs
    }
  }    
)


#' Create a fitted loadComp object.
#' 
#' Generates a new model of class loadComp (\code{\link{loadComp-class}}). 
#' loadComps themselves contain two inner load models, one for regression and 
#' one for interpolation of the residuals of the regression predictions.
#' 
#' @include getResiduals.R
#' @importFrom methods new
#' @param reg.model The model, usually a regression model, to whose predictions 
#'   the residuals corrections should be added.
#' @param interp.format character specifying the load format in which residuals 
#'   should be interpolated
#' @param interp.data the dataset, possibly differing from 
#'   getFittingData(reg.model), from which regression residuals will be 
#'   calculated and interpolated.
#' @param interp.function a function accepting args dates.in, y.in, and 
#'   dates.out and returning y.out. See \link{interpolations} for pre-defined
#'   options, or write your own having the same form.
#' @param abs.or.rel.resids Should residuals be computed as the difference 
#'   ("absolute") or the ratio ("relative") of the observed and predicted 
#'   values?
#' @param use.log logical. Should residuals be computed in log space (TRUE) or 
#'   linear space (FALSE)?
#' @param MSE.method character. The method by which the model should be 
#'   bootstrapped. "non-parametric": resample with replacement from the original
#'   fitting data, refit the model, and make new predictions. "parametric":
#'   resample the model coefficients based on the covariance matrix originally
#'   estimated for those coefficients, then make new predictions.
#' @param store One or more character strings specifying which information to 
#'   write within the model. Options are 'data': the original fitting data; 
#'   'fitting.function': a fitting function that can produce a new loadComp 
#'   object from new data (this currently uses the same new data for both 
#'   regression calibration and interpolation); 'uncertainty': an estimate of 
#'   uncertainty, which can take some time to compute but will permit creation 
#'   of uncertainty intervals, etc. in the prediction and aggregation phases.
#' @inheritParams estimateMSE.loadComp
#' @return A fitted loadComp model.
#' @export
#' @family load.model.inits
loadComp <- function(reg.model, 
                     interp.format=c("flux","conc"), abs.or.rel.resids=c("absolute","relative"), use.log=TRUE, 
                     interp.data, interp.function=linearInterpolation, store=c("data","fitting.function","uncertainty"),
                     n.iter=100, MSE.method="parametric") {
  
  # Validate arguments
  interp.format <- match.arg.loadflex(interp.format)
  abs.or.rel.resids <- match.arg.loadflex(abs.or.rel.resids)
  store <- match.arg.loadflex(store, c("data","fitting.function","uncertainty"), several.ok=TRUE)
  
  # Determine new metadata from several possible sources of info
  metadata <- getMetadata(reg.model) # start with metadata attached to reg.model
  
  # CM step 1 - get residuals
  resids <- getResiduals(load.model=reg.model, flux.or.conc=interp.format, abs.or.rel.resids=abs.or.rel.resids, use.log=use.log, newdata=interp.data)$Resid
  
  # CM step 2 - train interpolation model on residuals
  resid_model <- loadInterp(
    interp.format=interp.format, interp.function=interp.function, 
    data=data.frame(interp.data, Resid=resids), 
    metadata=if(interp.format=="flux") {
      updateMetadata(metadata=metadata, load.rate="Resid")
    } else {
      updateMetadata(metadata=metadata, constituent="Resid")
    },
    retrans.function=NULL,
    store=intersect(store, c("data","fitting.function")))
  
  # If requested, generate fitting function
  if("fitting.function" %in% store) {
    fitting_function <- function(training.data, store=c()) {
      # create & return new loadComp object based on a refitted regression model
      loadComp(reg.model=getFittingFunction(reg.model)(training.data, store=store), 
               interp.format=interp.format, abs.or.rel.resids=abs.or.rel.resids, 
               interp.data=training.data, interp.function=interp.function, store=store)
    }
  }
  
  # create new loadComp object
  load.model <- new(
    "loadComp",
    fit=new("compModel", reg.model=reg.model, resid.model=resid_model, log.resids=use.log, abs.or.rel.resids=abs.or.rel.resids),
    pred.format=interp.format,
    metadata=metadata,
    data=if("data" %in% store) interp.data else NULL,
    fitting.function=if("fitting.function" %in% store) fitting_function else NULL)
  
  # If requested, compute & store uncertainty from the loadComp object
  if("uncertainty" %in% store) {
    load.model@MSE <- estimateMSE(load.model, n.iter=n.iter, method=MSE.method)
  }
  
  load.model
}


#' Make flux or concentration predictions from a loadComp model.
#' 
#' Makes instantaneous predictions (at the temporal resolution of 
#' \code{newdata}) from a fitted \code{\link{loadComp}} model. See 
#' \code{\link{predictSolute}} for details.
#' 
#' @importFrom stats qnorm
#' @import dplyr
#' @inheritParams predictSolute
#' @param load.model A loadComp object.
#' @param newdata \code{data.frame}, optional. Predictor data, including any 
#'   predictor columns needed for the regression model and any needed for the 
#'   interpolation model. Column names should match those given in the 
#'   \code{loadComp} metadata. If \code{newdata} is not supplied, the original 
#'   fitting data will be used.
#' @param fit.reg logical. If TRUE, a column for the regression predictions 
#'   before interpolation will be included in the data.frame that is returned. 
#'   These will be in the same format (non-log, conc/flux) as the final 
#'   predictions.
#' @param fit.resid logical. If TRUE, a column for the residual corrections will
#'   be included in the data.frame that is returned. These will be in the same 
#'   format (non-log, conc/flux) as the final predictions, even when the 
#'   residuals were actually produced in log space and/or as relative residuals,
#'   so that fit = fit.reg + fit.resid.
#' @param fit.resid.raw logical. If TRUE, a column for the residual corrections 
#'   as returned from the interpolation model. These may be in log space and/or 
#'   unitless, depending on the type of residual correction specified when the
#'   loadComp model was created.
#' @return A vector of data.frame of predictions, as for the generic 
#'   \code{\link{predictSolute}}.
#' @export
#' @family predictSolute
predictSolute.loadComp <- function(
  load.model, flux.or.conc, newdata, interval=c("none","confidence","prediction"), 
  level=0.95, lin.or.log=c("linear","log"), se.fit=FALSE, se.pred=FALSE, date=FALSE, 
  attach.units=FALSE, agg.by=c("unit", "day", "month", "water year", "calendar year", "total", 
                               "mean water year", "mean calendar year", "[custom]"),
  fit.reg=FALSE, fit.resid=FALSE, fit.resid.raw=FALSE, ...) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  interval <- match.arg.loadflex(interval)
  attach.units <- match.arg.loadflex(attach.units)
  lin.or.log <- match.arg.loadflex(lin.or.log)
  agg.by <- match.arg(agg.by)
  match.arg.loadflex(fit.reg, c(TRUE, FALSE))
  match.arg.loadflex(fit.resid, c(TRUE, FALSE))
  match.arg.loadflex(fit.resid.raw, c(TRUE, FALSE))
  
  metadata <- getMetadata(load.model)
  
  # Make the prediction, always in the original (as-interpolated) format
  if(missing(newdata)) { 
    newdata <- getFittingData(load.model@fit@reg.model)
  }
  reg_preds <- predictSolute(
    load.model=load.model@fit@reg.model, flux.or.conc=load.model@pred.format, newdata=newdata, attach.units=attach.units, ...)
  resid_preds <- predictSolute(
    load.model=load.model@fit@resid.model, flux.or.conc=load.model@pred.format, newdata=newdata, attach.units=attach.units, ...)
  
  # Do the composite correction: combine reg_preds with resid_preds. resid_preds
  # may be logged or not, absolute or relative. reg_preds will always be in 
  # pred.format, non-log space. So convert resid_preds and the combination of 
  # reg_preds with resid_preds as needed. 4 possibilities.
  preds <- switch(
    load.model@fit@abs.or.rel.resids,
    "absolute"=(if(load.model@fit@log.resids) log(reg_preds) else reg_preds) + resid_preds,
    "relative"=(if(load.model@fit@log.resids) log(reg_preds) else reg_preds) * resid_preds)
  
  # If we aren't already in linear space, get there. No bias correction at this 
  # stage because (1) It's not clear the interpolation phase is biased (already
  # corrected in regression prediction phase), and (2) if we did bias correction
  # here, the observations and predictions wouldn't match even for
  # linearInterpolation and others that are supposed to run straight through
  # each residual.
  predvec_lin <- if(load.model@fit@log.resids) exp(preds) else preds
  
  # Change flux/conc formats if appropriate. MSEs are already formatted.
  predvec_lin <- formatPreds(
    predvec_lin, from.format=load.model@pred.format, to.format=flux.or.conc, 
    newdata=newdata, metadata=load.model@metadata, attach.units=attach.units)
  
  # Add uncertainty if requested
  if(interval != "none" | se.fit | se.pred | lin.or.log == 'log') {
    # Do early checks to send useful messages if we can't supply the requested
    # type of uncertainty
    if(interval == "confidence") {
      stop("confidence intervals not implemented for loadComp")
    } 
    if(se.fit) {
      stop("se.fit not implemented for loadComp")
    }
    if(isTRUE(all.equal(dim(load.model@MSE), c(0,0)))) {
      warning("Uncertainty estimates are unavailable. Proceeding with NAs")
      load.model@MSE <- matrix(NA, nrow=2, ncol=2, dimnames=list(c('mean','sd'), c('conc','flux')))
    } else if(is.na(load.model@MSE["mean", flux.or.conc])) {
      stop("Uncertainty estimates are unavailable for ",flux.or.conc,". Try fitting the model with data that include discharge.")
    }
    
    # Compute uncertainty using a lognormal distribution if log.resids==TRUE, 
    # normal elsewise. Although I expect a lognormal distribution will make 
    # sense most often, the estimateMSE routine often (rightly) gives NaN values
    # for meanlog when log.resids==FALSE, because it is possible in that case 
    # for predictions to be negative (and therefore for their log to be NaN). 
    # This all points in favor of preferring log.resids==TRUE, which I was 
    # leaning toward anyway because in general load estimates ARE lognormal,
    # more or less.
    if(load.model@fit@log.resids) {
      # Compute uncertainty assuming a lognormal distribution of error around
      # each prediction. The MSE comes in log space when
      # load.model@fit@log.resids==TRUE. Since flux and conc are identical here,
      # it doesn't matter which column we use - just use the first.
      se_log <- sqrt(load.model@MSE["mean", 1]) 
      # The mean: always use se.pred (rather than se.fit) to calculate the mean in
      # log space.
      preds_log <- mixedToLog(meanlin=predvec_lin, sdlog=se_log)
      preds_lin <- logToLin(mslist=preds_log)
      
      # The intervals:
      if(interval == "prediction") {
        # degrees of freedom are not straightforward for the interpolation part of
        # the model, so use qnorm instead of qt to get quantiles.
        ci_quantiles <- qnorm(p=0.5+c(-1,1)*level/2)
        preds_log$lwr <- preds_log$meanlog + ci_quantiles[1]*se_log
        preds_log$upr <- preds_log$meanlog + ci_quantiles[2]*se_log
        preds_lin$lwr <- exp(preds_log$lwr) 
        preds_lin$upr <- exp(preds_log$upr)
      }
      # The SEs:
      if(se.pred) {
        preds_log$se.pred <- se_log
        preds_lin$se.pred <- preds_lin$sdlin
      }
    } else {
      # Compute uncertainty assuming a normal distribution of error around each
      # prediction
      se_lin <- sqrt(load.model@MSE["mean", flux.or.conc])
      preds_lin <- data.frame(meanlin=predvec_lin, sdlin=se_lin)
      preds_log <- linToLog(mslist=preds_lin) # we only need preds_log if lin.or.log='log'
      
      # The intervals:
      if(interval == "prediction") {
        # degrees of freedom are not straightforward for the interpolation part of
        # the model, so use qnorm instead of qt to get quantiles.
        ci_quantiles <- qnorm(p=0.5+c(-1,1)*level/2)
        preds_lin$lwr <- preds_lin$meanlin + ci_quantiles[1]*se_lin 
        preds_lin$upr <- preds_lin$meanlin + ci_quantiles[2]*se_lin
        preds_log$lwr <- log(preds_lin$lwr) 
        preds_log$upr <- log(preds_lin$upr)
      }
      # The SEs:
      if(se.pred) {
        preds_lin$se.pred <- preds_lin$sdlin
        preds_log$se.pred <- preds_log$sdlog
      }
    } 
    
    # Format the output
    preds_lin$sdlin <- NULL # we've copied this to the se.pred column if we wanted it
    names(preds_lin)[1] <- "fit" # name consistently with other predictSolute outputs
  } else {
    # If we're not returning any uncertainty info, format as a vector rather than as a data.frame
    preds_lin <- predvec_lin
  }
  
  # Add dates if requested
  if(date) {
    if(!is.data.frame(preds_lin)) {
      preds_lin <- data.frame(fit=preds_lin)
    }
    # prepend the date column
    preds_lin <- data.frame(date=getCol(load.model@metadata, newdata, "date"), preds_lin)
  }
  
  # Add intermediate predictions (regression, residuals in linear and/or
  # original form) if requested
  if(fit.reg | fit.resid | fit.resid.raw) {
    if(!is.data.frame(preds_lin)) {
      preds_lin <- data.frame(fit=preds_lin)
    }
    fit_reg <- if(fit.reg || fit.resid) {
      formatPreds(
        reg_preds, from.format=load.model@pred.format, to.format=flux.or.conc, 
        newdata=newdata, metadata=load.model@metadata, attach.units=attach.units)
    }
    fit_resid <- if(fit.resid) {
      preds_lin$fit - fit_reg
    }
    fit_resid_raw <- if(fit.resid.raw) {
      resid_preds
    }
    preds_lin <- data.frame(
      preds_lin,
      c(if(fit.reg) list(fit.reg=fit_reg),
        if(fit.resid) list(fit.resid=fit_resid),
        if(fit.resid.raw) list(fit.resid.raw=fit_resid_raw)))
  }
  
  preds <- preds_lin
  if(lin.or.log == "log") {
    if(is.data.frame(preds)) {
      preds$fit <- log(preds$fit) # this is NOT the mean in log space, but it's the only way to get residuals of 0 in log space
      preds$fit.meanlog <- preds_log$meanlog # include the mean in log space for a tiny bit more clarity
      preds$se.fit <- if(se.fit) NA else NULL
      preds$se.pred <- if(se.pred) preds_log$se.pred else NULL
      preds$lwr <- if(interval == "prediction") log(preds$lwr) else NULL
      preds$upr <- if(interval == "prediction") log(preds$upr) else NULL
    }
  }
  
  #use aggregate solute to aggregate to agg.by, but warn and return NA for uncertainty
  if(agg.by != "unit") {
    preds <- aggregateSolute(preds, metadata = getMetadata(load.model), agg.by = agg.by,
                             format = flux.or.conc, dates = getCol(load.model@metadata, newdata, "date"))
  }
  
  # Return
  return(preds)
}



#' Estimate uncertainty in a composite model.
#' 
#' Calculates and returns the mean squared errors (MSEs) in the units and 
#' transformation space (e.g., log space) of the left-hand side of the model fit
#' equation.
#' 
#' This method is leave-one-out cross validation (LOOCV) for the interpolation
#' and involves repeated resampling of the coefficients from which the
#' regression predictions and residuals are calculated.
#' 
#' @importFrom stats sd
#' @inheritParams estimateMSE
#' @param n.iter The number of times to repeat the COMPLETE process of [simulate
#'   predictions from the regression model and do leave-one-out cross validation
#'   (for all interpolation data points)]. Each run through the process 
#'   generates one estimate of the MSE, from which a mean and SD of the MSE 
#'   estimates will be returned.
#' @param method  character. The method by which the model should be 
#'   bootstrapped. "non-parametric": resample with replacement from the original
#'   fitting data, refit the model, and make new predictions. "parametric":
#'   resample the model coefficients based on the covariance matrix originally
#'   estimated for those coefficients, then make new predictions.
#' @param rho The first-order autocorrelation coefficient to assume in 
#'   simulateSolute(regression.model, interpolation.data). If missing, rho will 
#'   be estimated from the interpolation data, but be warned that many data 
#'   points are needed to make this estimation with precision.
#' @export
#' @family estimateMSE
estimateMSE.loadComp <- function(load.model, n.iter=100, method="parametric", rho, ...) {
  
  # Pull out the resid.model for easier access
  resid.model <- load.model@fit@resid.model
  
  # Use the interpolation data for simulation, observation, and calculation of new residuals
  interp_data <- resid.model@data
  
  # Observe values from the same dataset
  interp_obs <- observeSolute(
    data=interp_data, # use the interpolation data, this time as a source of observations
    flux.or.conc=resid.model@pred.format, # use the interpolation model's interp.format format
    metadata=load.model@fit@reg.model@metadata, # use metadata from the regression model to access columns relevant to that model (i.e., not Resid, but the real flux/conc)
    calculate=NA, # only calculate if we need to
    attach.units=FALSE) # save time by skipping the units
  
  # Put the observations into log space if necessary for calculating residuals
  # and final errors
  if(load.model@fit@log.resids) {
    interp_obs_resids <- log(interp_obs)
  } else {
    interp_obs_resids <- interp_obs
  }
  
  # Pre-calculate the conversion factors to make preds and obs in "conc" or "flux" format. Calls to formatPreds are expensive, so do them infrequently.
  is_flux_format <- resid.model@pred.format == "flux"
  if(is_flux_format) {
    conc_factor <- formatPreds(rep(1, nrow(resid.model@data)), from.format=resid.model@pred.format, 
                               to.format="conc", newdata=resid.model@data, metadata=resid.model@metadata, attach.units=FALSE)
  } else {
    flux_factor <- formatPreds(rep(1, nrow(resid.model@data)), from.format=resid.model@pred.format, 
                               to.format="flux", newdata=resid.model@data, metadata=resid.model@metadata, attach.units=FALSE)
  }
  
  # Loop, each time resampling coefficients and calculating the leave-n-out 
  # (probably LOO) uncertainty for the interpolation.
  complete_MSE <- do.call(
    "rbind", #dimnames=list(NULL, c("conc","flux")), 
    lapply(1:n.iter, function(i) {
      
      # Simulate values from the regression, putting them into interpolation format (flux or conc) and linear space
      sim_preds <- simulateSolute(
        load.model@fit@reg.model, flux.or.conc=resid.model@pred.format, 
        newdata=interp_data, method=method, from.interval="confidence", if(!missing(rho)) rho)
      
      # Calculate residuals. How we calculate them should depend on the load.model
      # options. This code is almost identical to a chunk in getResiduals but 
      # repeated here because I don't see a need to generalize getResiduals enough
      # to handle this composite-method-specific simulation/LOOCV process.
      if(load.model@fit@log.resids) {
        sim_preds <- log(sim_preds)
      }
      sim_resids <- switch(
        load.model@fit@abs.or.rel.resids,
        "absolute" = interp_obs_resids - sim_preds,
        "relative" = interp_obs_resids / sim_preds)
      
      # Update the interpolation model with the new residuals
      interp_refit <- load.model@fit@resid.model
      # This method of updating interp_refit, going straight to the guts, is a 
      # little risky. It rests on the assumption that loadComp models always will 
      # pass loadInterps the data in the interpolation format, such that 
      # loadInterp will never have adjusted the values of y.in during the 
      # loadInterp() call. For now, though, that's true. And it saves us the time 
      # cost of reconverting dates, creating a new S4 object from scratch, and so
      # on.
      interp_refit@fit@y.in <- sim_resids 
      
      ### Get the MSE of the new interpolation model. Use LOOCV (delete-one
      ### jackknife): leave just one out at a time, iterating through all the
      ### elements of interp_data
      
      # Pull some model pieces from the model for quick access
      interpfun <- resid.model@fit@interp.function # the innermost interpolation function, e.g., linearInterpolation
      dates.numeric <- resid.model@fit@dates.in # dates.in == dates.out for MSE estimation; already converted in loadInterp() to numeric(POSIXct()).
      y.obs <- resid.model@fit@y.in
      
      # Leaving one observation out at a time, do the interpolation repeatedly.
      # Go straight to the inner interpfun because we've needed to "refit" the
      # interpolation model with new y.obs values but we'd rather not reconvert
      # the dates all over again
      resid_preds <- sapply(1:length(y.obs), function(left.out) {
        interpfun(dates.numeric[-left.out], y.obs[-left.out], dates.numeric[left.out])
      })
      
      # resid_preds may be logged or not; sim_preds has already been converted
      # to log space to match. resid_preds may be absolute or relative; the
      # combination function (+ or *) changes accordingly.
      preds <- switch(
        load.model@fit@abs.or.rel.resids,
        "absolute"=sim_preds + resid_preds,
        "relative"=sim_preds * resid_preds)
      
      # Compute and package the residuals for return as a chunk in the
      # completeMSE sapply loop. Compute the residuals in linear space when
      # log.resids is FALSE - otherwise, we'll sometimes run into problems with
      # negative predictions, whose log is NaN.
      if(!load.model@fit@log.resids) {
        # When !log.resids, preds and interp_obs_resids are already in linear 
        # space. So we're all ready to calculate residuals, making conversions
        # to conc or flux as needed
        conc_resids <- (interp_obs_resids - preds) * if(is_flux_format) conc_factor else 1
        flux_resids <- (interp_obs_resids - preds) * if(is_flux_format) 1 else flux_factor
        # Calculate & return the squared errors for this LOOCV run. It's more 
        # computationally intensive, but quicker to program, to do as we're 
        # doing here: return all the raw values, one column per format, rather
        # than computing an intermediate summary statistic.
        cbind(conc_resids^2, flux_resids^2)
        
      } else {
        # When !log.resids, preds and interp_obs_resids are already in linear 
        # space. So we're all ready to calculate residuals. Residuals are the
        # same for flux and conc in log space - astonishing but true!
        # log(A*C)-log(B*C) = log(A)+log(C)-log(B)-log(C) = log(A)-log(B) with
        # no C!
        resids2 <- (interp_obs_resids - preds)^2
        cbind(resids2, resids2)
      }
      
    }))
  
  # Do some error checking so users know if their MSE is coming out with NAs
  if(any(is.na(complete_MSE))) {
    warning('getting NAs when calculating composite model error; check for NA or negative values in the input data')
  }
  
  # Return the distribution of the MSE from all those leave-n-out interations
  # in a 2x2 matrix. The errors have have been calculated are already in the
  # right log/linear space and flux/conc format, so our only remaining job is
  # to summarize their distribution.
  MSEs <- rbind(mean=apply(complete_MSE, 2, mean), sd=apply(complete_MSE, 2, sd))
  colnames(MSEs) <- c("conc","flux")
  MSEs
}

#' Extract model summary statistics from a loadComp model
#' 
#' Produce a 1-row data.frame of model metrics. The relevant metrics for 
#' loadComp models include two sets of statistics about autocorrelation (one for
#' the regression residuals, one for the 'residuals' used to do the composite 
#' correction).
#' 
#' @md
#' @inheritParams summarizeModel
#' @param newdata data.frame of data that was/will be used to predict 
#'   concentration or load; should be the same as the \code{newdata} argument to
#'   \code{predictSolute}, e.g. a data.frame of daily or instantaneous dates and
#'   discharges.
#' @param irregular.timesteps.ok logical. If FALSE, this function requires that 
#'   the timesteps between observations are identical to one another, and a plot
#'   is generated and an error is thrown if this requirement is not met. If 
#'   TRUE, the check is not performed. If NA (the default), the check is 
#'   performed but the function proceeds with a warning and no plot if the 
#'   timesteps are found to be irregular. Tests and estimates of autocorrelation
#'   are weak to wrong when timesteps are irregular, but timesteps are often at 
#'   least a bit irregular in the real world.
#' @return Returns a 1-row data frame with the following columns:
#'   
#'   * `site.id` - the unique identifier of the site, as in [metadata()]
#'   
#'   * `constituent` - the unique identifier of the constituent, as in 
#'   [metadata()]
#'   
#'   * `RMSE.lin` or `RMSE.log` - the square root of the mean squared error, in
#'   log space (`RMSE.log`) if the `use.log` equalled `TRUE` in the call to 
#'   [loadComp()] that created this model.
#'   
#'   * `reg.durbin.watson` - the Durbin Watson test statistic as applied to the 
#'   residuals from the regression model fitting process
#'   
#'   * `reg.rho` - the autocorrelation coefficient of the residuals from the 
#'   regression model fitting process
#'   
#'   * `int.durbin.watson` - the Durbin Watson test statistic as applied to the 
#'   residuals from the interpolation model fitting process. See 
#'   [residDurbinWatson()]
#'   
#'   * `int.rho` - the autocorrelation coefficient of the residuals from the 
#'   interpolation model fitting process. See [estimateRho()]
#'   
#'   * `correction.frac` - the correction fraction, i.e., the fraction of total 
#'   concentration or flux prediction that is attributable to a correction such 
#'   as the residuals correction of composite method models. See 
#'   [getCorrectionFraction()]
#' @importFrom dplyr select everything
#' @export
#' @family summarizeModel
summarizeModel.loadComp <- function(
  load.model, newdata, irregular.timesteps.ok=NA, ...) {
  
  # prepare args we'll use a few times below
  resid.args <- list(
    load.model=getFittedModel(load.model)@reg.model,
    flux.or.conc=load.model@pred.format, 
    abs.or.rel.resids=getFittedModel(load.model)@abs.or.rel.resids, 
    use.log=getFittedModel(load.model)@log.resids,
    plot.acf=FALSE, irregular.timesteps.ok=irregular.timesteps.ok)
  resid.data <- getFittingData(getFittedModel(load.model)@resid.model)
  
  # create a data.frame of model metrics
  out <- NextMethod(load.model, ...) # site.id, constituent, etc.
  RMSE <- if(0 %in% dim(load.model@MSE)) {
    NA
  } else {
    sqrt(load.model@MSE["mean", resid.args$flux.or.conc])
  }
  if(resid.args$use.log) out$RMSE.log <- RMSE else out$RMSE.lin <- RMSE
  out$reg.durbin.watson <- do.call(residDurbinWatson, resid.args)
  resid.args$irregular.timesteps.ok <- TRUE # we checked on the first call but will now skip to avoid replicate warnings
  out$reg.rho <- do.call(estimateRho, resid.args)$rho
  out$int.durbin.watson <- do.call(residDurbinWatson, c(list(newdata=resid.data), resid.args))
  out$int.rho <- do.call(estimateRho, c(list(newdata=resid.data), resid.args))$rho
  out$correction.frac <- getCorrectionFraction(load.model, flux.or.conc=resid.args$flux.or.conc, newdata=newdata)
  
  # return
  return(out)
}

#' The fraction of prediction that is due to a correction.
#' 
#' Computes the fraction of total concentration or flux prediction that is 
#' attributable to a correction such as the residuals correction of composite 
#' method models.
#' 
#' @name getCorrectionFraction
#' @rdname getCorrectionFraction
#' @param load.model The load model from which to pull the correction fraction.
#' @param flux.or.conc character. Should the metric be computed with respect to
#'   predictions of flux rate or concentration?
#' @param newdata The data for which to compute the correction fraction, usually
#'   containing daily or instantaneous discharge.
#' @param ... Other arguments passed to class-specific implementations of 
#'   getCorrectionFraction.
#' @return A value between 0 and 1 indicating the fraction of total load that is
#'   attributed to a correction.
#' @export
#' 
#' @family diagnostics
getCorrectionFraction <- function(load.model, flux.or.conc=c("flux","conc"), newdata, ...) {
  UseMethod("getCorrectionFraction")
}

#' @description
#' 
#' \code{\link{loadComp}} models: The correction fraction for a composite method
#' (\code{loadComp}) model is defined here as:
#' 
#' \deqn{CF = (\sum |R| \Delta t) / (\sum L \Delta t)}
#' 
#' where \eqn{\sum} indicates a sum over all predictions, \eqn{L} is the vector
#' of composite predictions of flux or concentration, \eqn{R} is the vector of
#' interpolated residuals in the same format (flux or concentration, non-log
#' space) as \eqn{L}, and \eqn{\Delta t} is the vector of time periods 
#' represented by the predictions.
#' 
#' @rdname getCorrectionFraction
#' @importFrom stats complete.cases
#' @inheritParams getCorrectionFraction
#' @param na.rm logical. Should predictions with NA values be excluded?
#' @export
getCorrectionFraction.loadComp <- function(load.model, flux.or.conc=c("flux","conc"), newdata, na.rm=FALSE, ...) {
  if(missing(newdata)) stop("required argument to getCorrectionFraction: newdata")
  preds <- predictSolute(load.model, flux.or.conc, newdata, fit.reg=TRUE, fit.resid=TRUE, date=TRUE)
  if(isTRUE(na.rm)) {
    preds <- preds[complete.cases(preds), ]
  }
  R <- preds$fit.resid
  L <- preds$fit
  #dt involves explicit and/or implicit conversions to numeric; fine as long as
  #dates are in a date format with a decent as.numeric implementation (includes
  #Date, POSIXt, chron)
  dt <- as.numeric(c(diff(preds$date), mean(diff(preds$date))))
  
  sum(abs(R*dt))/sum(L*dt)
}
