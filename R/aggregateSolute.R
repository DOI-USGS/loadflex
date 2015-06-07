#' Aggregate loads by the time periods specified by the user
#' 
#' This will aggregate the total loads or mean concentrations per aggregation 
#' interval, specified by \code{agg.by}. The time frame specified by 
#' \code{agg.by} can be "unit," "day," "month," "water year," "calendar year," 
#' "total," or the name of a column in newdata that can be used to group the 
#' data.
#' 
#' This also calculates the uncertainty in the sum over a regular time series 
#' (loads) with known standard errors (loadsSEs) for each short-term load 
#' estimate.
#' 
#' The general equation for propagation of error in a sum of potentially 
#' autocorrelated values is:
#' 
#' sum_t(var(x[t])) + 2*sum_a,b(cov(x_a,a_t+l))
#' 
#' where we will assume something about the covariance matrix.
#' 
#' However, we will deviate from the above equation to accommodate the lognormal
#' distribution of each flux prediction.
#' 
#' @importFrom dplyr %>% group_by_ summarise
#' @importFrom lubridate tz
#' @importFrom smwrBase waterYear
#' @importFrom unitted u v get_units
#' @param preds Either a vector of predicted instantaneous fluxes or 
#'   concentrations or a data.frame containing the columns "fit", "se.pred", and
#'   "date"
#' @param metadata A metadata object describing the model
#' @param format character. The desired format of the aggregated values. If 
#'   "conc", preds is assumed to already be formatted as "conc". If "flux" or 
#'   "flux rate", preds is assumed to already be formatted as "flux rate". If 
#'   preds has a "units" attribute, that attribute is checked for consistency 
#'   with \code{format} and \code{metadata}, but if preds has no "units" 
#'   attribute then no such checks can be made.
#' @param agg.by character. The date interval or grouping column by which to 
#'   aggregate. If agg.by="unit", values will be returned unaggregated but in 
#'   the standard post-aggregation format. If agg.by is one of "day", "month", 
#'   "water year", or "calendar year", the dates vector will be split into 
#'   periods corresponding to those intervals, and the flux or concentration 
#'   will be computed for each period. If agg.by="total", \code{dates} will be 
#'   ignored and the entire vector \code{preds} will be aggregated. If 
#'   agg.by="[custom]", aggregation will occur for each unique value in 
#'   \code{dates}.
#' @param se.preds A vector of standard errors of prediction for instantaneous 
#'   flux or concentration predictions. This data may also be given as a column 
#'   named "se.pred" in preds when preds is a data.frame.
#' @param dates A vector, of the same length as preds, containing the dates to 
#'   aggregate over. This data may also be given as a column named "date" in 
#'   preds when preds is a data.frame.
#' @param custom An optional data.frame of one or more columns each containing 
#'   factors or other labels on which to aggregate. The columns to be used are 
#'   set by \code{agg.by}.
#' @param cormat.function A function that takes a vector of datetimes (Date, 
#'   POSIXct, chron, etc.) and returns a Matrix indicating the assumed/estimated
#'   correlation between prediction errors on each pair of datetimes. See 
#'   \link{correlations-2D} for predefined options.
#' @param ci.agg logical. Should confidence intervals for the aggregate 
#'   predictions be returned?
#' @param level numeric. The interval to span with the confidence intervals.
#' @param deg.free numeric. The degrees of freedom to use in calculating 
#'   confidence intervals from SEPs. If NA, a normal distribution is used rather
#'   than the more standard t distribution.
#' @param ci.distrib character. The distribution to assume for uncertainty in 
#'   the aggregate flux or concentration distribution. The default is 
#'   "lognormal".
#' @param se.agg logical. Should standard errors of the aggregate predictions be
#'   returned?
#' @param na.rm logical. Should NA values be ignored during aggregation (TRUE), 
#'   or should NA be returned for intervals that contain one or more NA 
#'   predictions (FALSE)?
#' @param attach.units logical. If true, units will be attached as an attribute 
#'   of the second column of the returned data.frame.
#'   
#' @return A data.frame with two columns. The first contains the aggregation 
#'   period or custom aggregation unit and is named after the value of 
#'   \code{agg.by}. The second contains the aggregate flux or concentration 
#'   estimates and is named after the value of \code{format}. The values in the 
#'   second column will be in the units specified by \code{metadata}.
#'   
#' @examples
#' metadata_example <- exampleMetadata()
#' preds_example <- data.frame(fit=abs(rnorm(365, 5, 2)), se.pred=abs(rnorm(365, 1, 0.2)), 
#'   date=seq(as.Date("2018-05-15"), as.Date("2019-05-14"), by=as.difftime(1, units="days")))
#' aggregateSolute(preds_example, metadata=metadata_example, format="conc", agg.by="month")
#' 
#' # with a custom aggregation group
#' preds_regrouped <- transform(preds_example, simple.season=ordered(
#'   c("winter","spring","summer","fall")[floor(((as.numeric(strftime(date, "%m"))+0)%%12)/3)+1], 
#'   c("winter","spring","summer","fall")))
#' aggregateSolute(preds_example, metadata=metadata_example, format="conc", 
#'                 agg.by="simple.season", custom=preds_regrouped)
#' 
#' # with a custom prediction error correlation matrix
#' new_correlation_assumption <- getCormatFirstOrder(rho=0.9, time.step=as.difftime(1, units="days"), max.tao=as.difftime(10, units="days"))
#' aggregateSolute(preds_example, metadata=metadata_example, format="conc", agg.by="month",
#'                 cormat.function=new_correlation_assumption)
#' 
#' @export
aggregateSolute <- function(
  preds, metadata, format=c("conc", "flux rate", "flux total"), 
  agg.by=c("unit", "day", "month", "water year", "calendar year", "total", "[custom]"),
  se.preds, dates, custom=NA, 
  cormat.function=cormat1DayBand,
  ci.agg=TRUE, level=0.95, deg.free=NA, ci.distrib=c("lognormal","normal"), se.agg=TRUE,
  na.rm=FALSE, attach.units=FALSE) {

  # Validate arguments
  format <- match.arg.loadflex(format, c("conc", "flux rate", "flux total"))
  attach.units <- match.arg.loadflex(attach.units)
  for(abi in 1:length(agg.by)) {
    agg.by[abi] <- match.arg.loadflex(agg.by[abi], c("unit", "day", "month", "water year", "calendar year", "total", colnames(custom)))
  }
  agg.by <- .reSpace(agg.by,"_") # replace spaces with underscores to use agg.by as a column name
  if(!is(custom,"data.frame")) {
    if(!is.na(custom)) {
      stop("Custom must be NA or a data.frame")
    }
  } else {
    numpreds <- if(is.data.frame(preds)) nrow(preds) else length(preds)
    if(nrow(custom) != numpreds) {
      stop("When custom is a data.frame, it must have as many rows as there are values in preds, se.preds, etc.")
    }
    colnames(custom) <- .reSpace(colnames(custom),"_") # do this after the match.arg.loadflex call
  }
  ci.distrib <- match.arg.loadflex(ci.distrib, c("lognormal","normal"))
  if(is.data.frame(preds)) {
    dates <- preds$date
    se.preds <- preds$se.pred
    preds <- preds$fit
  }
  
  # Check that dates contains actual dates
  if(!(is(dates, "POSIXt") | is(dates, "Date") | is(dates, "chron"))) {
    stop("Unexpected format for dates - must be POSIXt, Date, or chron")
  }
  
  # If possible, check the units of preds against the units implied by format and metadata
  pred_units <- get_units(preds)
  if(!is.na(pred_units)) {
    expected_pred_units <- switch(
      format,
      "conc"=metadata@conc.units,
      "flux rate"=metadata@load.rate.units,
      "flux total"=metadata@load.rate.units
    )
    if(pred_units != expected_pred_units) {
      stop(paste0("The units of preds should be ", expected_pred_units, 
                  ", given the metadata and format==", format))
    }
  }
  
  # Cohn WRR 2005, section 5, indicates that "one usually approximates the 
  # integral [of loads]" as a sum of sums, where the inner sums are days or 
  # hours and the outermost sum is over the entire period of interest. This is
  # quite conceptually different from pre-aggregating the predictors before
  # making the predictions, and it is something that I would like to implement
  # in a flexible way. See issue #109.
  #   if(isTRUE(preaggregate)) {
  #     if(all(agg.by %in% c("Month", "Water Year", "Calendar Year", "Total"))) {
  #       preaggregation <- aggregateSolute(
  #         preds, se.preds, format, metadata, dates, custom, agg.by="Day",
  #         cormat.function, preaggregate=FALSE, ci.agg, level, se.agg, na.rm, attach.units)
  #       preds
  #       se.preds
  #       dates
  #     }
  #   }
  
  # Decide on the aggregation vector (the usual case) or list of vectors
  # (uncommon, but possible for "custom")
  if(length(agg.by) == 1 & all(agg.by %in% c("unit","day","month","water_year","calendar_year","total"))) {
    aggregate_by <- setNames(
      data.frame(
        switch(
          agg.by,
          "unit"=1:length(preds),
          "day"=strftime(dates, "%Y-%m-%d", tz=tz(dates)),
          "month"=strftime(dates, "%Y-%m", tz=tz(dates)),
          "water_year"=waterYear(dates),
          "calendar_year"=strftime(dates, "%Y", tz=tz(dates)),
          "total"=rep(1,length(preds)),
          stop("")
        )),
      agg.by)
  }  else {
    aggregate_by <- custom[agg.by]
  }
  
  SEofSum <- function(dates, se.preds) {
    # By Cohn 2005 Equation 51, Var[L-muL] = 
    # sum_j(sum_i(rho_ij*mui*muj*(exp(sigma^2)-1))) where sigma^2 is the
    # variance in log space and mui is the mean in linear space (mui = exp(mulog
    # + 0.5*sigma^2)). In other words, this is the standard equation relating 
    # correlation to covariance as cov=cor*se_i*se_j. We can use the same 
    # equation, except that the SEs of the instantaneous loads have already been
    # calculated for us as the values returned when you call predictSolute with 
    # se.pred=TRUE. So it's sum_j(sum_i(cor_ij*se_i*se_j)).
    
    # Newey and West argued that autocovariance at longer lags should be
    # downweighted in calculating the variance of the sum, largely because it's
    # really hard to calculate the variance at a lag close to the length of the
    # time series (because there are only a few pairs of points with that lag
    # distance); this is therefore a small sample problem. So if you choose, you
    # can multiply each term by a weight specified by weight.fun. The default is 1
    # for any value of lag.num, but another good choice is 1-(l/(L+1)) as 
    # suggested by Newey and West (1987); other weights were suggested in papers 
    # subsequent to that one. If you do use such a weighting function, you should
    # choose L wisely; it should probably be constant across all aggregation
    # periods rather than changing from period to period, and it should probably
    # depend on the temporal resolution of your predictions. The weight function
    # is sometimes also called a kernel function.
    #   weight.fun=function(lag.num) { rep(1, length(lag.num)) }
    #   acfvec <- acfvec * weight.fun(0:num.lags)
    
    # First compute the correlations among observation errors, based on
    # assumptions or estimates embodied in cormat.function
    cor_matrix <- cormat.function(dates)
    
    # The covariance matrix is the element-wise product of cor_matrix and 
    # varvar_matrix, where varvar_matrix[i,j] is se.preds[i]*se.preds[j]. 
    # Because what we really want is the sum of all terms in the covariance 
    # matrix, and because varvar_matrix could be huge, we don't actually ever 
    # create varvar_matrix. Instead we use sparse matrix multiplication to get 
    # us first to another sparse matrix (half_cov_matrix) and then to a column 
    # vector (sums by row of products), and then we take the sum of that vector
    # straight away.
    half_cov_matrix <- cor_matrix*se.preds
    cov_matrix_sum <- sum(half_cov_matrix %*% se.preds)
    
    # At this point (between creating cov_matrix and computing the summation,
    # actually), the AMLE algorithm in rloadest adds an additional term for
    # covariance arising from coefficient uncertainty - see src/TAC_LOAD.f - but
    # for other model types this additional covariance is captured in se.pred
    # and the chosen cormat.function already.
    
    # Return the calculated SE
    sqrt(cov_matrix_sum)/length(se.preds)
  }
  agg_preds <- v(data.frame(preds, se.preds, dates, aggregate_by)) %>%
    group_by_(.dots=as.list(agg.by)) %>%
    dplyr::summarise(Value=mean(preds), SE=if(se.agg | ci.agg) SEofSum(dates, se.preds) else NA) %>%
    as.data.frame()
  
  ### Notes on Uncertainty ### 
  
  # We may find that a faster way to estimate the uncertainty in this sum 
  # is by Monte Carlo simulation, using the means and variances (or se.preds) of 
  # instantaneous fluxes to parametrically resample from those distributions, 
  # find the sum, and repeat until we have a population of sums from which we 
  # can estimate a distribution. See, for example, 
  # http://eprints.sics.se/2253/1/SICS-T--2002-01--SE.pdf ("Evaluating the CDF 
  # for m weighted sums of n correlated lognormal random variables", Lars 
  # Rasmusson, 2002, Swedish Institute of Compute Science, Report T2002:01,
  # ISRN: SICS-T-2002/01-SE, ISSN:110-3154)
  
  # If format="flux total", put flux into load.units
  if(format=="flux total") {  
    # For "flux total" we need to identify the duration of each period so we can
    # find total = rate * duration. The following lines should work if dates is 
    # a chronological sequence of dates, sort(aggregate_by)==aggregate_by, and 
    # each aggregation period flows smoothly into the next (without unusually 
    # large or small gaps). That's a lot of assumptions, and possibly not all of
    # them, so consider this code block preliminary.
    interval_bounds <- aggregate(list(start_date=v(dates)), by=aggregate_by, FUN=min)
    last_interval <- dates[aggregate_by[[1]] == interval_bounds[nrow(interval_bounds),1]]
    start_dates <- c(interval_bounds$start_date, max(last_interval) + mean(diff(last_interval))) # assume last interval ends one average timestep after its last time point
    days_per_interval <- as.numeric(diff(start_dates, units="days"))
    if(isTRUE(any(days_per_interval <= 0))) warning("Highly suspicious (<=0) Durations inferred from sequence of dates and aggregation groups")
    agg_preds$Duration <- u(days_per_interval, "days")
    # adjust both the prediction and the SE for each aggregation interval
    if(se.agg) {
      agg_preds[,c("Value","SE")] <- agg_preds[,c("Value","SE")] * days_per_interval
    } else {
      agg_preds[,"Value"] <- agg_preds[,"Value"] * days_per_interval
    }
  }
  
  # Compute prediction intervals if requested. 
  if(ci.agg) {
    if(ci.distrib == "lognormal") {
      # This calculation is appropriate to lognormally distributed loads or
      # concentrations. This code is modified from rloadest::predLoad, with
      # thanks to David Lorenz of the USGS.
      SE_log <- sqrt(log(1 + (agg_preds$SE/agg_preds$Value)^2)) # calculate the log-space SE
      mean_log <- log(agg_preds$Value) - 0.5*SE_log^2 # calculate the log-space mean
      # rloadest computes CI_quantile with a t distribution. When we don't know
      # the degrees of freedom, we will use a normal distribution rather than a
      # t.
      if(!is.na(deg.free)) {
        CI_quantile <- qt(1 - (1 - level)/2, df=deg.free)
      } else {
        CI_quantile <- qnorm(1 - (1 - level)/2)
      }
      agg_preds$CI_lower <- exp(mean_log - CI_quantile*SE_log)
      agg_preds$CI_upper <- exp(mean_log + CI_quantile*SE_log)
    } else { # ci.distrib == "normal"
      # This calculation is appropriate to normally distributed loads or concs. 
      # rloadest computes CI_quantile with a t distribution. When we don't know
      # the degrees of freedom, we will use a normal distribution instead.
      if(!is.na(deg.free)) {
        CI_quantile <- qt(1 - (1 - level)/2, df=deg.free)
      } else {
        CI_quantile <- qnorm(1 - (1 - level)/2)
      }
      agg_preds$CI_lower <- agg_preds$Value - CI_quantile*agg_preds$SE
      agg_preds$CI_upper <- agg_preds$Value + CI_quantile*agg_preds$SE
    } 
  }
  
  # If requested, determine the new units for the conc/flux/fluxrate columns.
  # Other columns (e.g., Period, Duration) are either non-unitted or already
  # have units attached.
  if(attach.units) {
    new_units <- switch(
      format,
      "conc"=metadata@conc.units,
      "flux rate"=metadata@load.rate.units,
      "flux total"=metadata@load.units
    )
    agg_preds <- u(agg_preds, replace(rep(NA, ncol(agg_preds)), names(agg_preds) %in% c("Value","SE","CI_lower","CI_upper"), new_units))
  } else {
    # Shake off any pre-existing units - e.g., those attached to Duration
    agg_preds <- v(agg_preds)
  }

  # Give the data.frame nice column names
  names(agg_preds)[1] <- .reSpace(.sentenceCase(names(agg_preds)[1]), "_")
  names(agg_preds)[match("Value", names(agg_preds))] <- .reSpace(.sentenceCase(format), "_")
 
  # Return
  agg_preds
}
