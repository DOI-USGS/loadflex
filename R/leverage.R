# This file will contain functions to calculate and report the leverage of each
# observation with respect to a specific load aggregation interval.

# Should we calculate leverage starting from scratch (pre-regression-model) or 
# starting after the model but before the residuals correction? I think both,
# because the user ought to be able to check the leverage of the regression
# model on its own, but sometimes the total leverage will also be interesting.

# Options for quantifying influence:
# Studentized residuals - no, because residuals of observations are 0 with the composite method
# Hat diagonal values - maybe, but because Yhat isn't a linear transformation of Y, the hat matrix is not calculable from the model coefficients. 
#                       Would it be possible to calculate it some other way? I believe the diagonal would still be all 1s.
# Leverage values - no, because generalized leverage is always 1 for the composite method with linear interpolation, so these are not informative.
# dffits - no, unless we can find equivalents for the standard error of the residuals and the hat matrix diagonals.
# dfbetas - no, because we're less interested in the betas than in the model predictions
# Cook's distance - no, unless I can find an equivalent for MSE that isn't 0 - also, the formula contains p, which may be inappropriate.
# covariance ratio - maybe...
# variance inflation factor (VIF) - maybe...

# After we figure out influence, there's also joint influence of several observations to consider

# From Efron 1992: Jackknife-After-Bootstrap Standard Errors and Influence Functions
# The jackknife influence function for a real-valued statistic s is
#   u_i{s} = (n-1) (s_() - s_(i))
# where
#   s_() = sum_i(s_(i)/n)

# As defined by Emerson (1984) and described by St. Laurent and Cook (1992):
# Generalized leverage: G(b;m) = (1/b)*(Yhatmb - Yhat)
#   i.e., the difference in fitted values between the models without and with a perturbation of size b on the mth observation, divided by b
# Jacobian leverage: J(m) = lim(b->0) G(b;m)
#   i.e., the limit of the generalized leverage as b approaches 0

#' Find the generalized leverage of observation m when perturbed by distance b.
#' 
#' Generalized leverage: G(b;m) = (1/b)*(Yhatmb - Yhat), i.e., the difference in
#' fitted values between the models without and with a perturbation of size b on
#' the mth observation, divided by b (as from St. Laurent and Cook, 1992)
#' 
#' You can have high leverage without having high influence; influence =
#' leverage * discrepancy.
#' 
#' @param b numeric value or vector. The distance(s) by which observation m 
#'   should be perturbed
#' @param m numeric value. The index of the observation in Y that should be 
#'   perturbed.
#' @param fit.and.predict.function function(X, Y) that takes a predictors matrix X and 
#'   an observation Y and returns the predictions Yhat for the observations X 
#'   given a model (in our case, usually a composite-method model) fitted to X and Y.
#' @param X an (n,p) matrix of predictors
#' @param Y an n-length vector of observations
#' @return return the leverage of m on X 
generalizedLeverage <- function(b, m, fit.and.predict.function, X, Y) {
  Yhat <- fit.and.predict.function(X, Y)
  Yhatmb <- matrix(NA, nrow=length(b), ncol=length(Y))
  for(i in 1:length(b)) {
    Ymb <- Y
    Ymb[m] <- Ymb[m] + b[i]
    Yhatmb[i,] <- fit.and.predict.function(X, Ymb) 
  }
  leverages <- (1/b)*(Yhatmb - matrix(Yhat, nrow=length(b), ncol=length(Y), byrow=TRUE))
  # If we really truly only want the leverage of m on m, the above lines are doing more work than necessary.
  leverage_m <- leverages[,m]
  return(leverage_m)
}

#' Covariance ratio
#' 
#' CR - det(s_i^2 * (X_i' %*% X_i)^-1) / det(s^2 * (X' %*% X)^-1)
#' 
#' What does this tell us about the predictions, as opposed to the coefficients?
#' 
#' I don't think this equation works; It's based on an assumption that the
#' predictions are directly related to the Xes, and it also requires an estimate
#' of the standard eror. Both of these are problematic to me at this point.


#' DFFITS # conceptually identical to cook's distance
#' 
#' DFFITS = (yhat_i - yhat_iI) / (s_I*sqrt(h_ii))
#' 
#' where I is (i) means "without observation i", yhat_i and yhat_iI are the
#' prediction for point i with and without point i included in the model, s_i is
#' the standard error estimated without point i, and hii is the leverage for the
#' point. (http://en.wikipedia.org/wiki/DFFITS)
#' 
#' h_ii is 1. What's s_I? That could be a problem.


# From Efron 1992: Jackknife-After-Bootstrap Standard Errors and Influence Functions
# The jackknife influence function for a real-valued statistic s is
#   u_i{s} = (n-1) (s_() - s_(i))
# where
#   s_() = sum_i(s_(i)/n)

#' Influence
#' 
#' I would like to find a reference to justify this particular approach, but I 
#' think the basic idea is about right: the influence of a particular 
#' observation on a flux estimate for a given period is the difference between 
#' the flux estimate with that observation and the flux estimate without.
#' 
#' @param fit.and.total.function function(...) that 
#'   takes a training predictors matrix X.calib, an observation Y.calib, a 
#'   prediction matrix X.est, and a character or factor vector P.est that 
#'   identifies the Period of each row of X.est. The function returns the 
#'   aggregated flux predictions Fhat for the observations X.est given a model
#'   (in our case, usually a composite-method model) fitted to X.calib and
#'   Y.calib.
#' @param X.calib a matrix of training predictors
#' @param Y.calib a vector of concentration observations
#' @param X.est a matrix of prediction predictors
#' @param P.est a vector of period designations - fit.and.total.function should 
#'   aggregate by these designations.
#' @return A vector of the same length as m with an influence measure for each 
#'   row index (of X and/or Y) in m.
fluxInfluence <- function(
  fit.function, obs.calib, obs.adjust, data.to.predict, correction.method="linear", replace.negatives=-1, aggregation.interval="unit",
  influence.on=c("both", "regression","adjustment")) {
  
  load.or.conc="load"
  match.arg(influence.on)
  
  Groups <- NA
  Yhatlist <- switch(
    influence.on,
    regression={
      if(!isTRUE(is.na(obs.adjust))) {
        warning("for influence.on='regression', obs.adjust is ignored")
      }
      # Simply get and aggregate the regression predictions
      lapply(seq_len(nrow(obs.calib)), function(m) {
        # Fit the model jackknife-style
        load.model <- fit.function(obs.calib[-m,])
        
        # Make predictions
        preds <- predictSoluteFromRegression(
          load.or.conc=load.or.conc, verbose=FALSE,
          load.model=load.model, 
          data.to.predict=data.to.predict)
        
        # Without dealing with residuals, just format the data so it can be passed to aggregateSolute
        adjusted_predictions <- transform(
          setNames(preds[,c("Date","Flow","Flux")],c("Date","Flow","OriginalFlux")),
          PredictedFlux=NA,
          LinearInterpObs=NA)
        if(!(aggregation.interval %in% c("unit","month","year","calendar year")) & aggregation.interval %in% names(data.to.predict)) {
          adjusted_predictions[,aggregation.interval] <- data.to.predict[,aggregation.interval]
        }
        
        # Do the aggregation as usual
        aggs <- aggregateSolute(
          load.or.conc=load.or.conc, verbose=FALSE,  
          load.model=load.model,
          loads=adjusted_predictions, 
          aggregation.interval=aggregation.interval)
        
        if(m==1) Groups <<- aggs$Period
        setNames(data.frame(m=aggs$RegressionFlux), m)
      })
    },
    adjustment={
      # Fit the model just once
      load.model <- fit.function(obs.calib)
      lapply(seq_len(nrow(obs.adjust)), function(m) {
        # Do residuals corrections jackknife-style (are residuals being determined by obs.adjust all the way through?)
        aggs <- predSoluteCM(
          load.or.conc=load.or.conc, load.model=load.model, data.to.predict=data.to.predict, observations=obs.adjust[-m,], 
          aggregation.interval=aggregation.interval, correction.method=correction.method, replace.negatives=replace.negatives, verbose=FALSE)
        
        if(m==1) Groups <<- aggs$Period
        setNames(data.frame(m=aggs$CompositeFlux), m)
      })
    },
    both={
      if(!all.equal(obs.adjust, obs.calib)) {
        stop("for influence.on='both', obs.adjust and obs.calib must be identical")
      }
      lapply(seq_len(nrow(obs.adjust)), function(m) {
        # Fit the model jackknife-style
        load.model <- fit.function(obs.calib[-m,])
        # Do residuals corrections jackknife-style (are residuals being determined by obs.adjust all the way through?)
        aggs <- predSoluteCM(
          load.or.conc=load.or.conc, load.model=load.model, data.to.predict=data.to.predict, observations=obs.adjust[-m,], 
          aggregation.interval=aggregation.interval, correction.method=correction.method, replace.negatives=replace.negatives, verbose=FALSE)
        
        if(m==1) Groups <<- aggs$Period
        setNames(data.frame(m=aggs$CompositeFlux), m)
      })
    })
  Yhatms <- do.call(cbind, Yhatlist)
  Yhatmsg <<- Yhatms
  
  # Return the jackknife influences for each row index in m
  n <- ncol(Yhatms)
  s_0 <- apply(Yhatms, MARGIN=1, FUN=sum)/n
  s_i <- Yhatms
  JKI <- (n-1)*(s_0 - s_i)/s_0 # Jackknife influences
  RJKI <- JKI/(apply(JKI^2, MARGIN=1, FUN=sum)/(n-1)) # RELATIVE jackknife influences
  cbind(Period=Groups, RJKI)  
}
