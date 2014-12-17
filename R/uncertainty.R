

#' Translate means and standard errors/deviations of lognormal distributions 
#' between log and linear space.
#' 
#' \code{logToLin} makes the transformation from the moments of a lognormal 
#' distribution in log space to the moments of the same distribution in linear 
#' space.
#' 
#' \subsection{Terminology}{
#' 
#' Let \code{L} be a sample from a lognormal distribution in linear space; for 
#' example, \code{L=rlnorm(1000)} could be a set of predicted flux rates in 
#' kg/day. Then let \code{N} be the corresponding set of predictions in log 
#' space, as they emerge from a model with a logged term on the left-hand side; 
#' for example, the output of \code{predict(lm(log(L_obs) ~ ...))} where 
#' \code{L_obs} is the set of observed flux rates used to calibrate the 
#' \code{lm} model. \code{N} is related to \code{L} by \code{N=log(L) or 
#' L=exp(N)}, but the parameters of their distributions are more complicated.
#' 
#' (It could also be that \code{L} is not a set of predicted flux rates but 
#' simply the uncertainty distribution of a single flux rate prediction. The 
#' same principles apply.)
#' 
#' Given the above definition,
#' 
#' \itemize{
#' 
#' \item \code{meanlin = mean(L) = mean(exp(N)) = exp(mean(N) + sd(N)^2/2)}
#' 
#' \item \code{sdlin = sd(L) = sd(exp(N)) = sqrt((exp(sd(N)^2) - 1) * 
#' exp(2*mean(N) + sd(N)^2))}
#' 
#' \item \code{meanlog = mean(N) = mean(log(L)) = log(mean(L)^2/sqrt(sd(L)^2 + 
#' mean(L)^2))}
#' 
#' \item \code{sdlog = sd(N) = sd(log(L)) = sqrt(log(1 + sd(L)^2/mean(L)^2))}
#' 
#' }
#' 
#' }
#' 
#' Note that meanlin does NOT simply equal exp(mean(N)), and sdlin does NOT 
#' equal exp(sd(N)); this function exists to apply those more complicated 
#' expressions on the right in the above list. Specifically, this function helps
#' you convert from \code{meanlog=mean(N)}, \code{sdlog=sd(N)} to an appropriate
#' mean and sd for \code{L} in linear space.
#' 
#' @name lognormal-moments
#' @param meanlog The mean of the distribution on the log scale, with the same
#'   meaning as the \code{meanlog} argument to \code{\link[stats]{rlnorm}}.
#' @param sdlog The standard deviation of the distribution on the log scale,
#'   with the same
#' @param mslist Optionally, a list containing the meanlog and sdlog as the 
#'   first and second elements. If this argument is specified, the first two 
#'   arguments should be left missing.
#' @export
logToLin <- function(meanlog, sdlog, mslist) {
  if(missing(meanlog) & missing(sdlog) & !missing(mslist)) {
    meanlog <- mslist[[1]]
    sdlog <- mslist[[2]]
  }
  data.frame(
    meanlin = exp(meanlog + sdlog^2/2),
    sdlin = sqrt((exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2)))
}

#' \code{linToLog} makes the opposite transformation, from the moments of a 
#' lognormal distribution in linear space to the moments of the same 
#' distribution in log space.
#' 
#' @rdname lognormal-moments
#' @param meanlin The mean of the lognormal distribution on the linear scale.
#' @param sdlin The standard deviation of the lognormal distribution on the 
#'   linear scale.
#' @export
linToLog <- function(meanlin, sdlin, mslist) {
  if(missing(meanlin) & missing(sdlin) & !missing(mslist)) {
    meanlin <- mslist[[1]]
    sdlin <- mslist[[2]]
  }
  data.frame(
    meanlog = log(meanlin^2/sqrt(sdlin^2 + meanlin^2)),
    sdlog = sqrt(log(1 + sdlin^2/meanlin^2)))
}

#' \code{mixedToLog} transforms from a mixed pair - either meanlin and sdlog, or
#' meanlog and sdlin - and returns meanlog and sdlog.
#' 
#' @rdname lognormal-moments
#' @export
mixedToLog <- function(meanlin, sdlog, meanlog, sdlin) {
  if(!missing(meanlin) & !missing(sdlog)) {
    data.frame(
      meanlog = log(meanlin) - sdlog^2/2,
      sdlog = sdlog)
  } else if(!missing(meanlog) & !missing(sdlin)) {
    data.frame(
      meanlog = meanlog,
      sdlog = NA) # I'm sure it can be done, but it seems too complicated to work out just now with no immediate use
  }
}

