
#### loadLm class ####

#' A load model class specific to simple linear models (\code{\link{lm}}s) for
#' flux estimation.
#' 
#' loadLms can take any lm formula.
#' 
#' @slot fit the interpolation model to be used.
#' @slot ylog logical. If TRUE, this constitutes affirmation that the values 
#'   passed to the left-hand side of the model formula will be in log space. If 
#'   missing, the value of \code{ylog} will be inferred from the values of 
#'   \code{formula} and \code{y.trans.function}, but be warned that this
#'   inference is fallible.
#' @importFrom methods setClass
#' @exportClass loadLm
#' @family load.model.classes
setClass(
  "loadLm", 
  
  slots=c( 
    fit="lm",
    ylog="logical"),
  
  contains="loadModel",
  
  # Validity of parent objects is checked first, automatically
  validity=function(object) {
    errorstrs <- character()
    
    validObject(object@fit)
    
    # Return
    if(length(errorstrs) == 0) {
      TRUE
    } else {
      errorstrs
    }
  }    
)



#' Create a fitted loadLm object.
#' 
#' Generates a new model of class loadInterp (\code{\link{loadLm-class}}) which 
#' can connect observations of concentration or flux using a linear regression 
#' model.
#' 
#' @importFrom methods new
#' @importFrom stats lm
#' @param formula A formula specifying the linear model to fit.
#' @param pred.format In what format (flux or conc) does the lm object make 
#'   predictions when predict.lm() is called?
#' @param data data.frame. The data to be interpolated
#' @param metadata metadata, used to access the appropriate columns of data. At 
#'   a minimum, \code{metadata} should correctly specify the date column.
#' @param retrans.function function that accepts a vector of predictions 
#'   straight from the fitted model and retransforms them, if appropriate, to 
#'   predictions in linear space and with units of concentration*flow. Because 
#'   load models are frequently fit to log(y) ~ ..., the default is 
#'   retrans.function=exp. After retrans.function has been applied to the 
#'   predictions, the retransformed predictions will automatically undergo 
#'   further units conversion from conc*flow to load.rate, according to the 
#'   metadata elements conc.units, flow.units, and load.rate.units.
#' @param store One or more character strings specifying which information to 
#'   write within the model. Options are 'data': the original fitting data; 
#'   'fitting.function': a fitting function that can produce a new loadComp 
#'   object from new data
#' @param fitting_function a fitting function that can produce a new loadComp 
#'   object from new data (this currently uses the same new data for both
#'   regression calibration and interpolation)
#' @param y.trans.function function that accepts a vector of predictions 
#'   straight from the fitted model and retransforms them, if appropriate, to 
#'   predictions in linear space and with units of concentration*flow. Because 
#'   load models are frequently fit to log(y) ~ ..., the default is 
#'   retrans.function=exp. After retrans.function has been applied to the 
#'   predictions, the retransformed predictions will automatically undergo 
#'   further units conversion from conc*flow to load.rate, according to the 
#'   metadata elements conc.units, flow.units, and load.rate.units.
#' @param ylog logical. If TRUE, this constitutes affirmation that the values 
#'   passed to the left-hand side of the model formula will be in log space. If 
#'   missing, the value of \code{ylog} will be inferred from the values of 
#'   \code{formula} and \code{y.trans.function}, but be warned that this
#'   inference is fallible.
#' @return A fitted loadLm model.
#' @export
#' @family load.model.inits
loadLm <- function(formula, pred.format=c("flux","conc"),
                   data, metadata, fitting_function=NULL, 
                   y.trans.function=NULL, retrans.function=exp,
                   store=c("data","fitting.function"), ylog) {
  # Validate arguments
  pred.format <- match.arg.loadflex(pred.format)
  store <- match.arg.loadflex(store, choices=c("data","fitting.function"), several.ok=TRUE)
  
  if(missing(ylog)) {
    # Give the benefit of the doubt until there's no confirmation of logged ys
    ylog <- TRUE
    # Check that y.trans.function or the formula suggest a log transformation
    if(!is.null(y.trans.function)) {
      if(!all.equal(y.trans.function, log)) {
        warning("y.trans.function should usually be NULL (if the formula has a log) or log")
        ylog <- FALSE
      }
    } else if(!any(as.list(as.list(formula)[[2]]) == as.symbol("log"))) {
      warning("formula should contain a log on the LHS unless y.trans.function is log")
      ylog <- FALSE
    }
    # Check that retrans.function == exp
    if(is.null(retrans.function)) {
      warning("Currently, retrans.function should be exp for loadLm models")
      ylog <- FALSE
    } else if(!all.equal(retrans.function, exp)) {
      warning("Currently, retrans.function should be exp for loadLm models")
      ylog <- FALSE
    }
  }
  
  # If requested, generate fitting function
  if("fitting.function" %in% store) {
    fitting_function <- function(training.data, store=c()) {
      loadLm(formula=formula, pred.format=pred.format, 
             data=training.data, metadata=metadata, 
             fitting_function=fitting_function, retrans.function=retrans.function, 
             store=store, ylog=ylog)
    }
  }
  
  # Create the model
  load.model <- new(
    "loadLm", 
    fit=lm(formula, data=data),   
    pred.format=pred.format,
    metadata=metadata,
    data=data,
    fitting.function=if("fitting.function" %in% store) fitting_function else NULL,
    retrans.function=retrans.function,
    ylog=ylog)
  
  load.model
}


#' Make flux or concentration predictions from a loadLm model.
#' 
#' Makes instantaneous predictions (at the temporal resolution of 
#' \code{newdata}) from a fitted \code{\link{loadLm}} model. See 
#' \code{\link{predictSolute}} for details.
#' 
#' @importFrom stats qnorm qt
#' @inheritParams predictSolute
#' @param load.model A loadLm object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names
#'   should match those given in the \code{loadLm} metadata. If
#'   \code{newdata} is not supplied, the original fitting data will be used.
#' @return A vector of data.frame of predictions, as for the generic 
#'   \code{\link{predictSolute}}.
#' @export
#' @family predictSolute
predictSolute.loadLm <- function(load.model, flux.or.conc=c("flux","conc"), newdata, 
                                 interval=c("none","confidence","prediction"), level=0.95,
                                 lin.or.log=c("linear","log"), se.fit=FALSE, se.pred=FALSE, 
                                 date=FALSE, attach.units=FALSE, ...) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  interval <- match.arg.loadflex(interval)
  attach.units <- match.arg.loadflex(attach.units)
  lin.or.log <- match.arg.loadflex(lin.or.log)
  
  # Check the model - can we confirm that y is logged?
  if(!load.model@ylog) {
    stop("For accurate prediction, y values must have been logged during model fitting")
  }
  
  # If there's no newdata, use the data
  if(missing(newdata)) {
    newdata <- load.model@data
  }
  
  # Get the direct model predictions from the fit object, adding intervals if 
  # requested and getting se.fit if either se.fit or se.pred is requested. (But
  # at least while loadLm requires logged left-hand sides, we will always need 
  # se.fit to retransform the mean.)
  preds_log <- predict(load.model@fit, newdata=newdata, interval=interval, se.fit=TRUE)

  # Take whatever format we got and put it into a data.frame
  if(is.matrix(preds_log)) {
    preds_log <- as.data.frame(preds_log) # e.g., interval="confidence" & se.fit=FALSE
  } else if(is.list(preds_log)) {
    if(is.matrix(preds_log[[1]])) {
      preds_log[[1]] <- as.data.frame(preds_log[[1]])
      preds_log <- data.frame(preds_log$fit, se.fit=preds_log$se.fit)
    } else {
      preds_log <- data.frame(fit=preds_log$fit, se.fit=preds_log$se.fit)
    }
  } else {
    preds_log <- data.frame(fit=preds_log)
  }

  # Calculate se.pred if needed. At least while loadLm only permits logged
  # left-hand sides, we will always need se.pred
  if(TRUE | se.pred) {
    # This calculation of se.pred is consistent with the predict.lm code.
    # Lines pulled & slightly modified from predict.lm:
    #       object <- load.model@fit
    #       p <- object$rank
    #       p1 <- seq_len(p)
    #       piv <- if (p) stats:::qr.lm(object)$pivot[p1]
    #       res.var <- sum(object$residuals^2)/object$df.residual
    #       # calculate ip
    #       X <- model.matrix(object)
    #       XRinv <- X[, piv] %*% qr.solve(qr.R(stats:::qr.lm(object))[p1, p1])
    #       ip <- drop(XRinv^2 %*% rep(res.var, p))
    #       # calculate pred.var
    #       weights <- 1
    #       pred.var <- res.var/weights
    #       # construct intervals
    #       level = 0.95
    #       tfrac <- qt((1 - level)/2, object$df.residual)
    #       hwid <- tfrac * switch(
    #         interval,
    #         confidence = sqrt(ip),
    #         prediction = sqrt(ip+pred.var))
    #       predictor <- cbind(preds$fit, preds$fit + hwid %o% c(1, -1))
    #       colnames(predictor) <- c("fit", "lwr", "upr")
    # therefore, ip = SE_mean and ip+pred.var=SE_pred.
    
    # Calculate se.pred^2 == se.fit^2 + pred.var, where pred.var == residual 
    # mean squared error (res.var/df) divided by weights, where we'll
    # assume/require for loadLm that weights are all 1.
    res.var <- sum(load.model@fit$residuals^2)/load.model@fit$df.residual
    if(!any(is.null(load.model@fit$weights))) stop("Weights must be null for now. Sorry.")
    weights <- 1
    pred.var <- res.var/weights
    # preds_log is always a data.frame by now, so just add a column
    preds_log$se.pred <- sqrt(preds_log$se.fit^2+pred.var)
  }
  
  # Apply the retransformation (which we currently require to be exp) to create
  # preds_lin from preds_log
  
  # Only perform retransformation if linear is desired
  if(lin.or.log == "log") {
    return(preds_log)
  }
  
  if(!all.equal(load.model@retrans.function, exp)) {
    # For now, restrict this function to loadLms where retrans.function == exp,
    # i.e., the model is fit to log-transformed y values.
    stop("predictSolute.loadLm currently only implemented for models where retrans.function == exp")
  }
  # This will be a standard conversion from SD of a lognormally distributed 
  # statistic to SD of a normally distributed one. If mu and sigma are the 
  # moments of the log(X) values (preds_log$fit and preds_log$se.fit (or se.pred?),
  # respectively), then back in linear space the moments of X are
  # preds_lin$fit=m=exp(mu+sigma^2/2) and preds_lin$se.fit^2=v=(exp(sigma^2) - 1)*exp(2*mu+sigma^2).
  
  # The mean: always use se.pred (rather than se.fit) to calculate the mean in 
  # linear space. logToLin is one way of doing a "bias correction", sensu Cohn 
  # 2005: it returns a meanlin of exp(meanlog+sdlog^2/2) instead of 
  # exp(meanlog). Other options would be to use the jackknife, smearing, or 
  # Finney's MVUE estimators; of those, MVUE has been repeatedly advocated by
  # Cohn and others, and something close to MVUE is encoded in rloadest.
  meansd_lin <- logToLin(meanlog=preds_log$fit, sdlog=preds_log$se.pred)
  preds_lin <- data.frame(fit=meansd_lin$meanlin)
  # The intervals: use a t distribution in log space, then reconvert to linear 
  # space. We could also use qlnorm, but that would use a normal rather than t 
  # in log space, and we do know the number of degrees of freedom. Regardless, 
  # this will be quite close to the output of qlnorm(p=0.5+c(-1,1)*level/2,
  # meanlog=preds_log$fit, sdlog=preds_log$se.pred).
  if(interval != "none") {
    se_ci <- switch(interval, confidence=preds_log$se.fit, prediction=preds_log$se.pred)
    DF <- load.model@fit$df.residual
    ci_quantiles <- qt(p=0.5+c(-1,1)*level/2, DF)
    preds_lin$lwr <- exp(preds_log$fit + ci_quantiles[1]*se_ci)
    preds_lin$upr <- exp(preds_log$fit + ci_quantiles[2]*se_ci)
  }
  # The SEs:
  if(se.fit) {
    preds_lin$se.fit <- logToLin(meanlog=preds_log$fit, sdlog=preds_log$se.fit)$sdlin
  }
  if(se.pred) {
    preds_lin$se.pred <- meansd_lin$sdlin
  }
  
  # Change flux/conc formats if appropriate
  preds_lin <- formatPreds(preds_lin, from.format=load.model@pred.format, to.format=flux.or.conc, 
                           newdata=newdata, metadata=load.model@metadata, attach.units=attach.units)
  
  # Add dates if requested
  if(date) {
    if(!is.data.frame(preds_lin)) {
      preds_lin <- data.frame(fit=preds_lin)
    }
    # prepend the date column
    preds_lin <- data.frame(date=getCol(load.model@metadata, newdata, "date"), preds_lin)
  }
  
  # If it's just the central predictions, return them as a vector rather than a data.frame
  if(ncol(preds_lin) == 1) {
    preds_lin <- preds_lin$fit
  }
  
  # Return
  return(preds_lin)
}

#' Resample the coefficients of a linear model (lm)
#' 
#' Returns a new linear model given their original covariance and uncertainty
#' 
#' (Although the name suggests otherwise, resampleCoefficients is not currently 
#' an S3 generic. You should refer to this function by its complete name.)
#' 
#' @importFrom stats coef rchisq
#' @importFrom MASS mvrnorm
#' @param fit an lm object whose coefficients should be resampled
#' @return A new lm object with resampled coefficients such that predict.lm()
#'   will make predictions reflecting those new coefficients. No other
#'   properties of the returned model are guaranteed.
#' @export
#' @references 
#' http://www.clayford.net/statistics/simulation-to-represent-uncertainty-in-regression-coefficients/
resampleCoefficients.lm <- function(fit) {
  
  # Extract useful info from the model. summary(fit)$cov.unscaled is equal to 
  # solve(crossprod(model.matrix(fit))), which looks like it could be more
  # direct and therefore faster, but I checked, using
  # autoplot(microbenchmark(summary(fit)$cov.unscaled, 
  # solve(crossprod(model.matrix(fit))))): the summary() method is faster.
  coefs <- coef(fit) # coefficients
  cov.unscaled <- summary(fit)$cov.unscaled # unscaled covariance matrix. 
  s.hat <- summary(fit)$sigma # residual standard error
  n.minus.k <- summary(fit)$df[2] # n - k
  
  # Simulate residual standard deviation as s.hat*sqrt(df/X) where s.hat is the 
  # estimate of sigma, df=n.minus.k is the degrees of freedom, and X "is a 
  # random draw from the chi-squared distribution with n-k degrees of freedom"
  # (www.clayford.net)
  s.hat.sim <- s.hat*sqrt(n.minus.k/rchisq(1, n.minus.k))
  
  # Simulate regression coefficients
  coef.resim <- mvrnorm(n=1, mu=coefs, Sigma=s.hat.sim^2*cov.unscaled)
  
  # Put the new coefficients into the model. It's $coefficients, not $coef, that
  # seems to matter for predict.lm(), so just do that one for efficiency's sake.
  fit$coefficients <- coef.resim
  
  # Return the lm object
  fit
}

#' Produce a set of predictions that reflect the coefficient uncertainty and 
#' natural variation.
#' 
#' This function resamples the coefficients from their joint distribution, then 
#' makes predictions whose individual errors are sampled from a time series with
#' the same first-order autocorrelation as the original series of errors.
#' 
#' @importFrom stats arima.sim sd
#' @inheritParams simulateSolute
#' @param load.model A loadLm object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names
#'   should match those given in the \code{loadLm} metadata. If
#'   \code{newdata} is not supplied, the original fitting data will be used.
#' @return A vector of data.frame of predictions, as for the generic 
#'   \code{\link{predictSolute}}.
#' @return A vector of predictions that are distributed according to the 
#'   uncertainty of the coefficients and the estimated natural variability + 
#'   measurement error.
#' @export
#' @family simulateSolute
simulateSolute.loadLm <- function(load.model, flux.or.conc=c("flux","conc"), newdata, 
                                  method=c("parametric", "non-parametric"), from.interval=c("confidence", "prediction"), rho, ...) {
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  from.interval <- match.arg.loadflex(from.interval, c("confidence","prediction"))
  method <- match.arg.loadflex(method, c("parametric", "non-parametric"))
  if(missing(newdata)) newdata <- getFittingData(load.model) # need to do this for predictSolute call in case non-parametric method overwrites load.model@data
  
  # Generate bootstrap model
  if(method=="parametric") {
    # Adjust the load.model@fit to contain the new (resampled) coefficients. 
    load.model@fit <- resampleCoefficients.lm(load.model@fit)
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
  
  # EXPERIMENTAL: Add noise to the predictions with an autocorrelation structure
  # based on that of the residuals (or supplied by the caller). To keep the
  # autocorrelation that's necessary for loadComp to make sense (even though
  # it's assumed not to exist in the calibration data), either (1) estimate
  # autocorrelation from the calibration data, or (2) accept an estimate of
  # autocorrelation from the user. Then use that autocorrelation and the
  # residual standard deviation to generate residuals.
  if(from.interval == "prediction") {
    if(missing(rho)) {
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
    s.hat <- summary(load.model@fit)$sigma
    noise <- noise * s.hat/sd(noise)
    # Add the noise back into the predictions, first converting back to linear space
    fitting.preds <- exp(log(fitting.preds) + noise)
  }
  
  return(fitting.preds)
}

#' Extract model summary statistics from a loadLm model
#' 
#' Produce a 1-row data.frame of model metrics. The relevant metrics for 
#' loadLm models include RMSE, p-values, and others TBD.
#' 
#' @inheritParams summarizeModel
#' @return A 1-row data.frame of model metrics
#' @importFrom dplyr select everything
#' @export
#' @family summarizeModel
summarizeModel.loadLm <- function(load.model, ...) {
  warning("summarizeModel.loadLm isn't implemented yet")
  data.frame(site.id=getMetadata(load.model)@site.id)
}
