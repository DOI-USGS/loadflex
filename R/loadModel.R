#### class definition ####

#' A generic load model class.
#'
#' Class and function definitions for very generic load models that you can
#' create with a single function call (\code{\link{loadModel}}) or extend to
#' more specific model types (e.g., \code{\link{loadInterp}},
#' \code{\link{loadReg2}}, \code{\link{loadComp}})
#'
#' @include metadata.R
#' @include loadModelInterface.R
#' @rdname loadModel-class
#' @name loadModel-class
#' @slot fit A statistical model, fit to the data and wrapped by the loadModel
#'   class for additional functionality specific to load models.
#' @slot pred.format A string indicating the format of predictions (flux or
#'   conc).
#' @slot metadata A metadata object describing the load model.
#' @slot data The fitting data for the model (fit).
#' @slot fitting.function The function used to create or recreate the loadModel,
#'   possibly with new fitting data.
#' @slot y.trans.function A function to be applied to the y variable before
#'   fitting the model.
#' @slot retrans.function A function to be applied to the y predictions before
#'   returning their values from \code{predictSolute()}.
#' @importFrom methods setClass
#' @exportClass loadModel
#' @family load.model.classes
setClass(
  "loadModel",
  slots=c(
    fit="ANY",
    pred.format="character", # Keeping separate from metadata because pred.format should be private
    metadata="metadata", # Essentially public; accessible by getMetadata()
    data="ANY",
    fitting.function="ANY",
    y.trans.function="ANY",
    retrans.function="ANY"),

  prototype=c(
    fit=NULL,
    pred.format="",
    metadata=new("metadata"),
    data=NULL,
    fitting.function=NULL,
    y.trans.function=NULL,
    retrans.function=NULL),

  # from the setClass documentation: "a validity-checking method for objects
  # from this class (a function that returns TRUE if its argument is a valid
  # object of this class and one or more strings describing the failures
  # otherwise)"
  validity=function(object) {
    errorstrs <- character()

    valid_pred_formats <- c("flux","conc*flow","conc")
    if(!(object@pred.format %in% valid_pred_formats)) {
      errorstrs <- c(errorstrs, paste("pred.format must be one of:", paste(valid_pred_formats, collapse=", ")))
    }

    # Return
    if(length(errorstrs) == 0) {
      TRUE
    } else {
      errorstrs
    }
  }
)


#### show ####

#' Display a loadModel object
#'
#' This will print a loadModel object to the console and is the default method
#' used for that process.
#'

#' @rdname show.loadModel
#' @param object loadModel object to be displayed.
#' @importFrom methods setMethod getSlots slot show
#' @importFrom utils head
#' @exportMethod show
#' @export

setMethod(
  "show", "loadModel",
  function(object) {
    cat("loadModel",if(class(object)[1] != "loadModel") paste0("(subclass ",class(object)[1],")"))
    for(sn in names(getSlots(class(object)))) {
      cat("\n***",sn,"***\n",sep="")
      if(sn=="data") {
        print(head(slot(object, sn)))
      } else {
        show(slot(object, sn))
      }
    }
  }
)


#### initialize ####

#' Create a fitted loadModel object.
#'
#' Generates a new model of class loadModel (\code{\link{loadModel-class}}).
#'
#' @importFrom methods new
#' @param inner.fit.function function that accepts one argument, a training data
#'   frame, and returns a [re]fitted statistical model, such as an \code{lm},
#'   relating flux or concentration to predictors in the training data.
#' @param pred.format character string that describes the output of
#'   retrans.function(predict(fit)) [NOT of predictSolute.loadModel(), which can
#'   do either!]
#' @param data data.frame containing the initial training observations
#' @param metadata An object of class \code{\link{metadata}}
#' @param y.trans.function function that accepts a vector of observed response
#'   values (e.g., concentrations or flux rates) and transforms them into the
#'   values on the left-hand side of the calibration formula. Because load
#'   models are frequently fit to log(y) ~ ..., the default is
#'   y.trans.function=log.
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
#' @return A fitted loadModel.
#' @export
#' @family load.model.inits
loadModel <- function(inner.fit.function, pred.format, data, metadata,
                      y.trans.function=NULL, retrans.function=exp,
                      store=c("data","fitting.function")) {

  store <- match.arg.loadflex(store, choices=c("data","fitting.function"), several.ok=TRUE)

  # need call here to transform data y before passing to inner.fit.function. But which column[s] is/are the y's?
  #data[,metadata@...uh...] <- y.trans.function(...uh...)

  new("loadModel",
      fit=inner.fit.function(data),
      pred.format=pred.format,
      metadata=metadata,
      data=if("data" %in% store) data else NULL,
      fitting.function=if("fitting.function" %in% store) {
        function(training.data, store=c()) {
          loadModel(inner.fit.function, pred.format, training.data, metadata,
                    y.trans.function, retrans.function, store=store)
        }
      } else NULL,
      y.trans.function=y.trans.function,
      retrans.function=retrans.function)
}

#### loadModelInterface ####

#' Retrieve metadata from a loadModel load model
#'
#' @inheritParams getMetadata
#' @export
#' @family getMetadata
getMetadata.loadModel <- function(load.model) {
  load.model@metadata
}


#' Retrieve the data used to fit the model
#'
#' @inheritParams getFittingData
#' @export
#' @family getFittingData
getFittingData.loadModel <- function(load.model) {
  load.model@data
}


#' Retrieve a fitting function from a loadModel load model
#'
#' @inheritParams getFittingFunction
#' @export
#' @family getFittingFunction
getFittingFunction.loadModel <- function(load.model) {
  load.model@fitting.function
}


#' Retrieve the fitted model, if appropriate, from a loadModel load model
#'
#' @inheritParams getFittedModel
#' @export
#' @family getFittedModel
getFittedModel.loadModel <- function(load.model) {
  load.model@fit
}


#' Make flux or concentration predictions from a loadModel model.
#'
#' Makes instantaneous predictions (at the temporal resolution of
#' \code{newdata}) from a fitted \code{\link{loadModel}} model. See
#' \code{\link{predictSolute}} for details.
#'
#' @importFrom stats predict
#' @inheritParams predictSolute
#' @param load.model A loadModel object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names
#'   should match those given in the \code{loadModel} metadata. If
#'   \code{newdata} is not supplied, the original fitting data will be used.
#' @return A vector of data.frame of predictions, as for the generic
#'   \code{\link{predictSolute}}.
#' @export
#' @family predictSolute
predictSolute.loadModel <- function(
  load.model, flux.or.conc=c("flux","conc"), newdata=getFittingData(load.model),
  date=TRUE, count=!identical(agg.by,"unit"), se.fit=FALSE, se.pred=FALSE,
  interval=c("none","confidence","prediction"), level=0.95, lin.or.log=c("linear","log"),
  agg.by=c("unit", "day", "month", "water year", "calendar year", "total", "[custom]"),
  na.rm=FALSE, attach.units=FALSE, ...) {

  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  interval <- match.arg.loadflex(interval)
  attach.units <- match.arg.loadflex(attach.units)
  lin.or.log <- match.arg.loadflex(lin.or.log)
  agg.by <- match.arg(agg.by)
  if(missing(count)) count <- !(identical(agg.by, "unit"))

  # If there's no newdata, use the data
  if(missing(newdata)) {
    newdata <- load.model@data
  }

  # Get the direct model predictions from the fit object
  preds <- predict(load.model@fit, newdata, ...)

  # Apply the user-supplied retransformation function
  if(!is.null(load.model@retrans.function)) {
    num.args <- length(as.list(args(load.model@retrans.function)))-1
    if(num.args == 1) {
      preds <- load.model@retrans.function(preds)
    } else if(num.args == 2) {
      preds <- load.model@retrans.function(preds, load.model@fit)
    } else {
      stop("retrans.function must accept either 1 or 2 arguments")
    }
    # How does error get propagated through the retrans.function? var(kX) = k^2
    # var(X)? what about var(exp(X))? The potential dependence on
    # retrans.function is part of the reason that confidence intervals, se.fit,
    # and se.pred are all unimplemented in this predictSolute.loadModel function.
  }

  # Add intervals if requested
  if(interval == "none") {
    preds <- formatPreds(preds, from.format=load.model@pred.format, to.format=flux.or.conc,
                         newdata=newdata, metadata=load.model@metadata, attach.units=attach.units)
  } else if(interval == "confidence") {
    stop("confidence intervals not implemented for generic loadModel")
  } else if(interval=="prediction") {
    # If there's an estimate of MSE, this can be done; see Cohn 2005 eq. 60 and 61.
    # By the lognormal assumption (61):
    #   sigmaLN = sqrt(log(1+MSE/L^2))
    #   muLN = log(L) - sigma^2/2
    #   CILN = exp(muLN + c(1,-1)*1.96*sigmaLN)
    stop("prediction intervals not implemented for generic loadModel")
  }

  # If se.fit==TRUE or se.pred==TRUE, add columns for those standard errors.
  # Lines commented out in this block could be useful in new implementations of
  # predictSolute.loadXX for new loadXX classes that extend loadModel and
  # implement the loadModelInterface.
  if(se.fit || se.pred) {
    ## UNCOMMENT THESE LINES for loadModel extensions ##
    # if(!is.data.frame(preds)) {
    #   preds <- data.frame(fit=preds)
    # }
    if(se.fit) {
      # preds$se.fit <- ## FILL HERE for loadModel extensions ##
      stop("se.fit not implemented for generic loadModel")
    }
    if(se.pred) {
      # preds$se.pred <- ## FILL HERE for loadModel extensions ##
      stop("se.pred not implemented for generic loadModel")
    }
  }

  # For loadModel extensions, add a rescaling of intervals / SEs either here or
  # above, making use of retrans.function and possibly also specific knowledge
  # of the particular retransformation to be done.

  # Add dates if requested
  if(date) {
    if(!is.data.frame(preds)) {
      preds <- data.frame(fit=preds)
    }
    # prepend the date column
    preds <- data.frame(date=getCol(load.model@metadata, newdata, "date"), preds)
  }

  preds
}




#' Produce a set of predictions that reflect the coefficient uncertainty and
#' natural variation.
#'
#' This function resamples the coefficients from their joint distribution, then
#' makes predictions whose individual errors are sampled from a time series with
#' the same first-order autocorrelation as the original series of errors.
#'
#' @inheritParams simulateSolute
#' @param load.model A loadModel object.
#' @param newdata \code{data.frame}, optional. Predictor data. Column names
#'   should match those given in the \code{loadModel} metadata. If
#'   \code{newdata} is not supplied, the original fitting data will be used.
#' @return A vector of data.frame of predictions, as for the generic
#'   \code{\link{predictSolute}}.
#' @return A vector of predictions that are distributed according to the
#'   uncertainty of the coefficients and the estimated natural variability +
#'   measurement error.
#' @export
#' @family simulateSolute
simulateSolute.loadModel <- function(load.model, flux.or.conc=c("flux","conc"), newdata,
                                     method=c("parametric", "non-parametric"), from.interval=c("confidence", "prediction"), rho, ...) {

  # Restrict applicability; descendants of loadModel will make more sense for this in the future
  stop("simulateSolute.loadModel is unimplemented; use a descendant such as loadLm")

}

#' Extract model summary statistics from a loadModel model
#'
#' Produce a 1-row data.frame of model metrics. The relevant metrics for
#' loadModel models include two sets of statistics about autocorrelation (one for
#' the regression residuals, one for the 'residuals' used to do the composite
#' correction).
#'
#' @inheritParams summarizeModel
#' @return A 1-row data.frame of model metrics
#' @importFrom dplyr select everything
#' @export
#' @family summarizeModel
summarizeModel.loadModel <- function(load.model, ...) {
  out <- data.frame(
    site.id = getMetadata(load.model)@site.id,
    constituent = getMetadata(load.model)@constituent,
    stringsAsFactors=FALSE
  )
  return(out)
}
