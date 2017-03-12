#' Actions & checks for when the user [indirectly] calls loadReg()
#' 
#' Require that the user has loaded rloadest. Also give the user a message about
#' rloadest if appropriate.
#' 
#' @keywords internal
checkRloadestStatus <- function() {
  rloadest_loaded <- "package:rloadest" %in% search()
  if(!rloadest_loaded) {
    stop("to use loadReg2(), please call library(rloadest) first")
  } else if(!pkg_env$rloadest_msg_given) {
    message("You are fitting an rloadest model (loadReg). ",
            "Please remember to cite both citation('loadflex') and citation('rloadest').")
    pkg_env$rloadest_msg_given <- TRUE
  }
}

#' Extracts and imports metadata from an rloadest loadReg model into an object of class 
#' "metadata"
#' #' 
#' @inheritParams getMetadata 
#' @importFrom rloadest loadReg
#' @param load.model a loadReg object
#' @export
#' @return Object of class "metadata" with slots modified according to the
#'   metadata contained in load.model
#' @family getMetadata
getMetadata.loadReg <- function(load.model) {

  metadata(
    constituent=load.model$constituent,
    flow=load.model$flow,
    dates=load.model$dates,
    conc.units=load.model$conc.units, 
    flow.units=load.model$flow.units, 
    load.units=load.model$load.units, 
    load.rate.units=paste0(load.model$load.units, "/day"),
    site.name=load.model$station)

}


#' Resample the coefficients from a loadReg model.
#' 
#' Dives deep into loadReg objects to replace the coefficients for the purpose
#' of simulating new predictions.
#' 
#' @importFrom stats rchisq
#' @importFrom MASS mvrnorm
#' @param fit a loadReg object
#' @param flux.or.conc Should the resampling be done for the coefficients 
#'   appropriate to flux or those for concentration?
#' @return A new loadReg object with resampled coefficients such that one of 
#'   predConc or predLoad (corresponding to the value of \code{flux.or.conc}) 
#'   will make predictions reflecting those new coefficients. No other 
#'   properties of the returned model are guaranteed.
#' @export
resampleCoefficients.loadReg <- function(fit, flux.or.conc) {
  
  # Validate arguments
  match.arg.loadflex(flux.or.conc)
  
  # We only need to adjust the one model fit we're going to use
  onefit <- switch(
    flux.or.conc,
    flux=fit$lfit,
    conc=fit$cfit)
  
  ## Resample Coefficients ##
  
  # Like simple linear models, the vector of fitted coefficients for an AMLE 
  # model (called omega-hat in Cohn 2005) is distributed according to a 
  # multivariate normal, according to paragraph 28 of Cohn 2005. The parameters 
  # are mean M = Beta + Aarrow * sigma, variance = SIGMA = Carrow*sigma^2 (eq. 
  # 32) where Carrow = (Varrow - gamma(gamma')(Vomegahatomegahat))/N. These have
  # non-obvious names in the LOADEST code, but they're present.
  
  
  ## Extract useful info from the model

  # Calculate S2_U, C, and M (Cohn 2005 section 4.4) as done in TAC_LOAD.f
  PARMLE <- onefit$PARMLE # PARMLE[1:NPAR] == Beta_hat and PARMLE[NPAR+1] == sigma_hat^2, as used in Eq. 29.
  NPAR <- onefit$NPAR # The number of actual parameters, K in Eq. 29
  CV <- onefit$CV # "covar. of std normals censored at XSI's w.r.t. S**2"
  SCV <- onefit$SCV # "covar. of standard normals censored at XSI's w.r.t. S". somethign special is stored in SCV[NPAR+1,NPAR+1]. this is the version used to calculate C, and is Varrow in eq. 33.
  NOBSC <- onefit$NOBSC # equal to the N used in Cohn 2005 - number of calibration observations
  # P24: B_Betaihat = Bias_1[Betaihat]/sigma and B_sigmahat = Bias_1[sigma_hat]/sigma
  BIAS <- onefit$BIAS
  SBIAS <- onefit$SBIAS # maybe SBIAS = fit$BIAS/NOBSC, where BIAS == the B_arrow of text after Eq 31?
  S2 <- PARMLE[NPAR+1]
  # DO 10 I=1,NPAR
  #   GAMMA(I) = SCV(I,NPAR+1)/SCV(NPAR+1,NPAR+1)    # Text between Eqs 29 & 30
  GAMMA <- SCV[1:NPAR,NPAR+1]/SCV[NPAR+1,NPAR+1]
  #   OMEGA(I) = PARMLE(I)-GAMMA(I)*SQRT(S2)         # Eq 29
  OMEGA <- PARMLE[1:NPAR]-GAMMA*sqrt(S2)
  #   B(I) = SBIAS(I)-GAMMA(I)*(1.D0+SBIAS(NPAR+1))  # B(I) == the A_arrow of text after Eq 31?
  # 10   CONTINUE
  # DO 30 I=1,NPAR
  #   DO 20 K=1,NPAR
  #     C(I,K) = SCV(I,K)-SCV(NPAR+1,NPAR+1)*GAMMA(I)*GAMMA(K)
  #   20      CONTINUE
  # 30   CONTINUE
  C <- SCV[1:NPAR,1:NPAR] - SCV[NPAR+1,NPAR+1]*(GAMMA %*% t(GAMMA))
  # S2_U = S2/(1.D0+BIAS(NPAR+1))
  S2_U <- S2/(1+BIAS[NPAR+1]) # S2_U (the AMLE estimate) is used for sigma^2 in eq. 59. should work for eq. 32, too.
  
  
  ## This part parallels the resampling of a regular linear model (lm) and is 
  ## adapted from 
  ## http://www.clayford.net/statistics/simulation-to-represent-uncertainty-in-regression-coefficients/
  
  coefs <- OMEGA
  cov.scaled <- C * S2_U # scaled covariance matrix
  cov.unscaled <- C # unscaled covariance matrix
  s.hat <- sqrt(S2_U) # residual standard error, AMLE estimate
  # n - k is supposed to be degrees of freedom. eq. 36 has df = v = 
  # 2N*((1+Bs2/N)^2/Vs2s2). If I understand right that the calculation of S2_U 
  # (S2_U <- S2/(1+BIAS[NPAR+1]), above) is the same equation as in paragraph 
  # 29, then Bs2/N is stored in BIAS[NPAR+1]. Vs2s2 is probably 
  # CV[NPAR+1,NPAR+1], though I'd like more evidence to be sure. To be like an 
  # lm, n.minus.k should be close to 23 for the app2 data (less if there were 
  # censored data). What I get with 2*NOBSC*((1+BIAS[NPAR+1])^2 / 
  # CV[NPAR+1,NPAR+1]) happens to be exactly the square of 23...so...take the 
  # sqrt? I guess I will...Ultimately, though, we could head for sigma^2=v*sA^2 
  # / chi_v^2 which chi_v^2 is a random variable with the given chi-square 
  # distribution, v degrees of freedom. Alternatively, Eqn. 41 gives a second relation 
  # between sA^2 and sigma^2, solveable for sigma using the Pythagorean theorem
  # because the LHS, b1, and b2 are known. Still, surely this is close enough for a
  # first cut.
  n.minus.k <- sqrt(2*NOBSC*((1+BIAS[NPAR+1])^2 / CV[NPAR+1,NPAR+1]))
  
  # Simulate residual standard deviation. Is it still the case, even with the 
  # altered distribution of s and sigma, that sigma.hat.sim (==s.hat.sim) is chi
  # square with df=n.minus.k? Could be close but not perfect. rloadest::EXPON.f 
  # assumes "S2 is a GAMMA(ALPHA,SIGMA^2*KAPPA) random variable", but then, S2
  # is not SIGMA2, and SIGMA2 is the s.hat^2 we really want.
  s.hat.sim <- s.hat*sqrt(n.minus.k/rchisq(1, n.minus.k))
  
  # Simulate regression coefficients
  omega.sim <- mvrnorm(n=1, mu=coefs, Sigma=s.hat.sim^2*cov.unscaled)
  
  
  ## Now work the coefficients back into their places in the loadReg$Xfit (onefit) object
  
  # To adjust the model enough that new predictions reflect the resampled 
  # coefficients, we need to make sure that PARMLE, BIAS, CV, SBIAS, and SCV are
  # up to date (because these are the variables used by 
  # predLoad/estlday/TAC_LOAD to calculate MVUE, which are the point load 
  # estimates). The other arguments to TAC_LOAD are NPAR and XLPRED; of those,
  # NPAR should stay the same, and XLPRED will be computed by the predLoad or
  # predConc call from the new predictor data, so we don't need to worry about
  # them.
  
  # Put PARMLE back together with new coefficients and S2
  PARMLE <- omega.sim - GAMMA*sqrt(S2)
  S2 <- s.hat.sim * (1+BIAS[NPAR+1])
  onefit$PARMLE <- c(PARMLE, S2)
  # Even though TAC_LOAD, etc. use PARMLE, coef.loadReg ~= coef.censReg uses
  # PARAML. Within rloadest, PARAML is also used for plot.loadReg and
  # print.loadReg. It's quick enough, I guess...let's replace it here.
  onefit$PARAML <- onefit$PARMLE
  
  # Leave BIAS, CV, SBIAS, and SCV untouched for prediction of new points. 
  # Prediction of intervals doesn't matter; simulateSolute offers no uncertainty
  # information. But TAC_LOAD also uses these for reprediction (in particular, 
  # for the g_v(...) term in eq. 42) - BIAS and CV go into the calculation of 
  # ALPHA and KAPPA, and SBIAS and SCV go into the calculation of GAMMA, A1, and
  # B1. All that said, I think their original values are the ones we want: bias 
  # and the covariance structure of the coefficients shouldn't change just
  # because we resample the coefficients.
  
  # Put the updated onefit back into a loadReg object
  if(flux.or.conc=="flux") {
    fit$lfit <- onefit
  } else {
    fit$cfit <- onefit
  }
  
  # Return the loadReg object
  fit
  
}

#' @section Metrics:
#'   
#'   \describe{
#'   
#'   \item{\code{Intercept}, \code{lnQ}, \code{DECTIME}, \code{sin.DECTIME}, 
#'   \code{cos.DECTIME}, etc.}{Coefficient estimates for the named terms. The 
#'   model formula determines which terms are included. Recall that log(Q) was 
#'   centered before the coefficients were fit.}
#'   
#'   \item{\code{RMSE}}{The root mean squared error of the difference between 
#'   observed and predicted values of concentration.}
#'   
#'   \item{\code{r.squared}}{The proportion of variation explained by the 
#'   model.}
#'   
#'   \item{\code{p.value}}{}
#'   
#'   }
#' @rdname summarizeModel.loadReg2
#' @param flux.or.conc character. Which internal model (the flux model or the 
#'   concentration model) should be summarized? A \pkg{rloadest} model, and 
#'   therefore also a \code{loadReg2} model, is actually two different models 
#'   for (1) flux and (2) concentration, each fitted to the same data and with 
#'   the same model structure except for whether the left-hand side of the model
#'   formula is flux or concentration. Some of the model metrics differ between 
#'   these two internal models.
#' @importFrom smwrStats rmse
#' @importFrom utils capture.output
#' @importFrom stats coef
#' @export
summarizeModel.loadReg <- function(load.model, flux.or.conc=c("flux", "conc"), ...) {
  
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  loadReg.model <- c("flux"="load","conc"="concentration")[[flux.or.conc]]
  loadReg.fit <- load.model[[c(flux='lfit',conc='cfit')[[flux.or.conc]]]]
  
  # summarize the coefficient estimates, standard errors, and p-values
  coefs <- coef(load.model, summary=TRUE, which=loadReg.model)
  coefsDF <- setNames(
    data.frame(
      t(coefs[,'Estimate']),
      SE=t(coefs[,'Std. Error']),
      pval=t(coefs[,'p-value'])
    ),
    nm = paste0(
      rep(gsub('(Intercept)', 'Intercept', row.names(coefs), fixed=TRUE), 3), # coefficient names
      rep(c('', '.SE', '.p.value'), each=nrow(coefs))) # aspect of coefficient being described
  )
  
  # package coefs and other overall statistics into a single 1-row data.frame
  retDF <- data.frame(
    eqn = capture.output(loadReg.fit$call[[2]]),
    RMSE = rmse(load.model, model=loadReg.model),
    r.squared = loadReg.fit$RSQ, # R-square needs to change when censored values are present!! see print.loadReg.R line 131 in rloadest. is this adjusted?
    p.value = rlmetricPVal(loadReg.fit),
    cor.resid = loadReg.fit$SCORR,
    PPCC = rlmetricPPCC(loadReg.fit),
    coefsDF,
    stringsAsFactors=FALSE
  )
  return(retDF)
}

#' Compute the p-value of a loadReg fit
#' 
#' Helper function to compute the p-value like rloadest does in print.loadReg
#' @importFrom stats pchisq
#' @param fit the loadReg lfit or cfit object
#' @keywords internal
rlmetricPVal <- function(fit) {
  G2 <- signif(2*(fit$LLR - fit$LLR1), 4)
  pval <- 1 - pchisq(G2, fit$NPAR - 1)
  return(pval)
}

#' Compute the probability plot correlation coefficient of a loadReg fit
#' 
#' Helper function to compute the probability plot correlation coefficient as 
#' rloadest does in print.loadReg, lines 141-144
#' @importFrom smwrQW censPPCC.test as.lcens
#' @importFrom stats residuals
#' @param fit the loadReg lfit or cfit object
#' @keywords internal
rlmetricPPCC <- function(fit) {
  if(fit$method == 'AMLE') {
    Res <- residuals(fit, type="working", suppress.na.action=TRUE)
    ppcc <- censPPCC.test(as.lcens(Res, censor.codes=fit$CENSFLAG))
    return(unname(ppcc$statistic))
  } else {
    NA # PPCC cannot be computed for method MLE
  }
}
