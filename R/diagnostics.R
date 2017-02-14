#### Assessing autocorrelation ####

#' Check a time series for evenly spaced dates.
#' 
#' Even spacing of dates is required for many tests and assessments of 
#' autocorrelation. This function checks for even spacing to within a specified 
#' tolerance. If spacing is uneven, a histogram is immediately produced (if 
#' \code{hist=TRUE}), and a handling function is run (e.g., to produce an error 
#' or warning, as specified by \code{handler}).
#' 
#' @importFrom ggplot2 ggplot
#' @param dates A time series of dates in any format accepted by \code{diff}.
#' @param hist logical. If the time series is found to be irregular, should this
#'   function print a histogram of the observed time steps?
#' @param handler A function, e.g., \code{stop} or \code{warning}, to be run if
#'   the time series is irregular.
#' @param tol time step tolerance, the accepatable amount of difference between time steps to still consider them regular.
#' @return TRUE if the time steps in \code{dates} are identical to within the 
#'   tolerance set by {tol}, FALSE otherwise.
#' @importFrom ggplot2 ggplot aes geom_histogram xlab
#' @export
isTimestepRegular <- function(dates, hist=TRUE, tol=.Machine$double.eps^0.5, handler=stop) {
  TimeInterval  <- '.ggplot.var'
  time_diffs <- diff(dates)
  is_irregular <- (length(unique(time_diffs)) > 1) & (diff(range(time_diffs)) > tol)
  if(is_irregular) {
    if(hist) {
      # Plot a histogram of time intervals for the user's inspection. The 
      # binwidth is the ggplot2 default, but set explicitly to avoid the warning
      suppressWarnings(print(
        ggplot(data.frame(TimeInterval=as.numeric(time_diffs)), aes(x=TimeInterval)) + 
          geom_histogram(binwidth=diff(range(as.numeric(time_diffs)))/30) + 
          theme_bw() + xlab(paste0("Time Interval (",units(time_diffs),")")) + ylab("Count of Time Interval")))
    }
    handler(paste("Time series is irregular.", if(hist) "Printing histogram of timesteps."))
  }
  return(!is_irregular)
}

#' Test for autocorrelation of residuals
#' 
#' Extracts residuals from a load.model (where residuals may be for the 
#' calibration data or for a new set of observations). Applies 
#' car::durbinWatsonTest to test for autocorrelation of those residuals.
#' 
#' @importFrom car durbinWatsonTest
#' @importFrom stats acf
#' @param load.model a loadModel descendant
#' @param flux.or.conc character. The format in which residuals should be 
#'   calculated
#' @param abs.or.rel.resids character. Should residuals be computed as the 
#'   difference or the ratio of the observed and predicted values?
#' @param use.log logical. use log residuals?
#' @param newdata The data from which to compute residuals; if NULL, the 
#'   original fitting data for load.model will be used.
#' @param plot.acf logical. Should the autocorrelation function be plotted?
#' @param timestep.tol the acceptable tolerance for considering timesteps 
#'   regular.
#' @param irregular.timesteps.ok logical. If FALSE, this function requires that 
#'   the timesteps between observations are identical to one another, and a plot
#'   is generated and an error is thrown if this requirement is not met. If 
#'   TRUE, the check is not performed. If NA (the default), the check is 
#'   performed but the function proceeds with a warning and no plot if the 
#'   timesteps are found to be irregular. Tests of autocorrelation are weak to
#'   wrong when timesteps are irregular, but timesteps are often at least a bit
#'   irregular in the real world.
#' @return A Durbin-Watson test statistic applied to residuals.
#' @export
#' 
#' @seealso car::durbinWatsonTest
#' @family diagnostics
residDurbinWatson <- function(load.model, flux.or.conc=c("flux","conc"), 
                              abs.or.rel.resids=c("absolute","relative"), use.log=FALSE, newdata=NULL, 
                              plot.acf=TRUE, timestep.tol=.Machine$double.eps^0.5, irregular.timesteps.ok=NA) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  abs.or.rel.resids <- match.arg.loadflex(abs.or.rel.resids)
  
  # Get the ordered, dated residuals of the model applied to newdata (where
  # newdata is replaced by fitting data in getResiduals if needed)
  resids <- getResiduals(
    load.model=load.model, 
    flux.or.conc=flux.or.conc, abs.or.rel.resids=abs.or.rel.resids, use.log=use.log, 
    newdata=newdata)
  
  # Assess the regularity of the time series (unless this checking has been overridden)
  if(is.na(irregular.timesteps.ok)) {
    is_regular <- isTimestepRegular(resids$Date, tol=timestep.tol, hist=FALSE, handler=warning)
  } else if(!irregular.timesteps.ok) {
    is_regular <- isTimestepRegular(resids$Date, tol=timestep.tol, hist=TRUE, handler=function(e) { invisible() })
    if(!is_regular) {
      stop("The Durbin-Watson test is invalid for an irregular time series. Set irregular.timesteps.ok=TRUE to continue anyway.")
    }
  }
  
  # Assess the range of residuals. For many composite models, the observable 
  # errors (residuals) will all be 0, which will cause the durbinWatsonTest to
  # find complete correlation.
  if(isTRUE(all.equal(diff(range(resids$Resid)), 0))) {
    warning("The Durbin-Watson test returns NaN when residuals are all identical")
  }
  
  # Produce a related plot if requested; should help in evaluating the autocorrelation
  if(plot.acf) {
    acf(resids$Resid, ylim=c(-1,1))
  }
  
  # Use the car package to test for autocorrelation of the residuals. Because
  # load.model is not always a linear model, we'll simply pass in the residuals
  # and will accept the lack of p-values in the return value
  durbinWatsonTest(model=resids$Resid)
}


#' Estimate the autocorrelation of a mid- to high-resolution time series
#' 
#' Uses arima() to estimate the first-order autocorrelation constant, rho, for 
#' the residuals calculated from a load model and new data.
#' 
#' For the purpose of estimating prediction error for a composite method 
#' application, this function should be called with a dataset of comparable (or 
#' identical) resolution to that of the interpolation data.
#' 
#' For the purpose of estimating the covariance of prediction errors for 
#' aggregation, this function should not be called unless newdata are at 
#' approximately the same (or higher) temporal resolution as that of the data to
#' be used for load estimation. This will almost always involve sensor data.
#' 
#' If you have no data with sufficient resolution to reasonably call this 
#' function, take your pick from the established assumptions implemented in 
#' functions such as \code{\link{rhoEqualDates}} or \code{\link{rho1DayBand}}, 
#' or write your own assumptions.
#' 
#' @importFrom stats acf arima coef
#' @param load.model a loadModel descendant
#' @param flux.or.conc The format in which residuals should be calculated
#' @param abs.or.rel.resids Should residuals be computed as the difference or 
#'   the ratio of the observed and predicted values?
#' @param use.log logical. use log residuals?
#' @param newdata prediction values. If this is set to NULL, 
#'   \code{getFittingData(load.model)} will be used.
#' @param plot.acf logical. Should the autocorrelation function be plotted?
#' @param timestep.tol the acceptable tolerance for considering timesteps 
#'   regular.
#' @param irregular.timesteps.ok logical. If FALSE, this function requires that 
#'   the timesteps between observations are identical to one another, and a plot
#'   is generated and an error is thrown if this requirement is not met. If 
#'   TRUE, the check is not performed. If NA (the default), the check is 
#'   performed but the function proceeds with a warning and no plot if the 
#'   timesteps are found to be irregular. Estimates of autocorrelation are weak
#'   to wrong when timesteps are irregular, but timesteps are often at least a
#'   bit irregular in the real world.
#' @return Return the rho function and the fitted model.
#' @export
#' 
#' @family diagnostics
estimateRho <- function(load.model, flux.or.conc=c("flux","conc"), 
                        abs.or.rel.resids=c("absolute","relative"), use.log=FALSE, newdata=NULL,
                        plot.acf=TRUE, timestep.tol=.Machine$double.eps^0.5, irregular.timesteps.ok=NA) {
  
  # Validate arguments - but these are both validated immediately within getResiduals
  #   flux.or.conc <- match.arg.loadflex(flux.or.conc)
  #   abs.or.rel.resids <- match.arg.loadflex(abs.or.rel.resids)
  
  # Get the ordered, dated residuals of the model applied to newdata
  resids <- getResiduals(
    load.model=load.model, 
    flux.or.conc=flux.or.conc, abs.or.rel.resids=abs.or.rel.resids, use.log=use.log, 
    newdata=newdata)
  
  # Assess the regularity of the time series (unless this checking has been overridden)
  if(is.na(irregular.timesteps.ok)) {
    is_regular <- isTimestepRegular(resids$Date, tol=timestep.tol, hist=FALSE, handler=warning)
  } else if(!irregular.timesteps.ok) {
    is_regular <- isTimestepRegular(resids$Date, tol=timestep.tol, hist=TRUE, handler=function(e) { invisible() })
    if(!is_regular) {
       stop("rho cannot be estimated for an irregular time series. Set irregular.timesteps.ok=TRUE to continue anyway.")
    }
  }
  
  # Produce a related plot if requested; should help in evaluating the autocorrelation
  if(plot.acf) {
    acf(resids$Resid)
  }
  
  # Estimate rho
  arima.model <- arima(resids$Resid, order=c(1, 0, 0), include.mean=FALSE)
  
  # Extract the first-order autocorrelation coefficient, rho
  rho <- coef(arima.model)[["ar1"]]
  
  # Record the timestep. Let's use the mode of the difftimes.
  time_diffs <- diff(resids$Date)
  Mode <- function(x) { as.numeric(names(which.max(table(x)))) }
  time_step <- as.difftime(Mode(time_diffs), units=units(time_diffs))
  
  # Return the rho function and the fitted model.
  list(
    rho=rho,
    time.step=time_step,
    rho.fun=getRhoFirstOrderFun(rho=rho, time.step=time_step),
    cormat.fun=getCormatFirstOrder(rho=rho, time.step=time_step), 
    resids=resids[c("Date","Resid")],
    arima.model=arima.model)
}
