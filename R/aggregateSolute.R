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
#' @importFrom dplyr %>% group_by_ summarise filter n n_groups
#' @importFrom lubridate tz
#' @importFrom smwrBase waterYear
#' @importFrom unitted u v get_units
#' @importFrom methods is
#' @importFrom stats aggregate qt qnorm setNames
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
#' @param dates A vector, of the same length as preds, containing the dates to
#'   aggregate over. This data may also be given as a column named "date" in
#'   preds when preds is a data.frame.
#' @param custom An optional data.frame of one or more columns each containing
#'   factors or other labels on which to aggregate. The columns to be used are
#'   set by \code{agg.by}.
#' @param agg.cols logical. Should the output data.frame include a column or
#'   columns specifying the aggregation group/s for each row? TRUE is
#'   recommended.
#' @param count logical. Should a count of the number of observations per group
#'   be included? For most values of agg.by, when count=TRUE there will be a new
#'   column called Count.
#' @param na.rm logical. Should NA values be ignored during aggregation (TRUE),
#'   or should NA be returned for intervals that contain one or more NA
#'   predictions (FALSE)?
#' @param attach.units logical. If true, units will be attached as an attribute
#'   of the second column of the returned data.frame.
#' @param ... Defunct and ignored arguments. Defunct arguments include
#'   'se.preds', 'ci.agg', 'deg.free', 'ci.distrib', 'se.agg', and
#'   'cormat.function'.
#' @return A data.frame with 2+ columns. The first column or set of columns
#'   contains the aggregation period or custom aggregation unit and is named
#'   after the value of \code{agg.by}. The second contains the aggregate flux or
#'   concentration estimates and is named after the value of \code{format}. The
#'   values in the second column will be in the units specified by
#'   \code{metadata}.
#'
#' @examples
#' \dontrun{
#' data(eg_metadata)
#' metadata_example <- updateMetadata(eg_metadata, dates="date")
#' preds_example <- data.frame(fit=abs(rnorm(365, 5, 2)),
#'   date=seq(as.Date("2018-05-15"), as.Date("2019-05-14"), by=as.difftime(1, units="days")))
#' aggregateSolute(preds_example, metadata=metadata_example, format="conc", agg.by="month")
#'
#' # with a custom aggregation group
#' preds_regrouped <- transform(preds_example, simple.season=ordered(
#'   c("winter","spring","summer","fall")[floor(((as.numeric(strftime(date, "%m"))+0)%%12)/3)+1],
#'   c("winter","spring","summer","fall")))
#' aggregateSolute(preds_example, metadata=metadata_example, format="conc",
#'                 agg.by="simple.season", custom=preds_regrouped)
#' }
aggregateSolute <- function(
  preds, metadata, format=c("conc", "flux rate"), 
  agg.by=c("unit", "day", "month", "water year", "calendar year", "total", "[custom]"),
  dates, custom=NA, na.rm=FALSE, attach.units=FALSE, agg.cols=TRUE, count=TRUE,
  ...) {
  
  # Check for defunct arguments
  dots <- names(eval(substitute(alist(...))))
  defunct_args <- dots[which(dots %in% c('se.preds','ci.agg','deg.free','ci.distrib','se.agg','cormat.function'))]
  if(length(defunct_args) > 0) {
    warning(sprintf(
      "ignoring these defunct argument%s: %s",
      if(length(defunct_args) > 1) 's' else '',
      paste(defunct_args, collapse=', ')
    ))
  }
  
  # Validate arguments
  if(format == "flux total") {
    warning("format=\"flux total\" is no longer supported Flux rate can be multiplied by duration to get total flux")
  }
  format <- match.arg.loadflex(format, c("conc", "flux rate"))
  attach.units <- match.arg.loadflex(attach.units)
  default_agg.by <- c("unit", "day", "month", "water year", "calendar year", "total")
  for(abi in 1:length(agg.by)) {
    agg.by[abi] <- match.arg.loadflex(agg.by[abi], c(default_agg.by, colnames(custom)))
  }
  agg.by <- .reSpace(agg.by,"_") # replace spaces with underscores to use agg.by as a column name
  if(is.data.frame(preds)) {
    # check for required columns
    need_col <- c('date', 'fit')
    missing_col <- need_col[!need_col %in% colnames(preds)]
    if(length(missing_col) > 0) 
      stop(paste0("missing column[s] ", paste0("'", missing_col, "'", collapse=' & '), " in the preds data.frame"))
    
    # extract columns into vectors and, if appropriate, a custom data.frame
    dates <- preds[,'date']
    preds <- preds[,'fit']
    if(is.na(custom) && length(setdiff(names(preds), c('fit','se.pred','date'))) > 0) {
      custom <- preds
    }
  }
  if(!is(custom, "data.frame")) {
    if(!is.na(custom)) {
      stop("Custom must be NA or a data.frame")
    }
  } else {
    if(nrow(custom) != length(preds)) {
      stop("When custom is a data.frame, it must have as many rows as there are values in preds")
    }
    colnames(custom) <- .reSpace(colnames(custom),"_") # do this after the match.arg.loadflex call
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
      "flux rate"=metadata@load.rate.units
    )
    if(pred_units != expected_pred_units) {
      stop(paste0("The units of preds should be ", expected_pred_units, 
                  ", given the metadata and format==", format))
    }
  }
  
  # Decide on the aggregation vector (the usual case) or list of vectors
  # (uncommon, but possible for "custom")
  if(length(agg.by) == 1 & all(agg.by %in% gsub(" ", "_", default_agg.by))) {
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
  
  # Group the estimates as requested
  preds_grp <- group_by_(
    v(data.frame(preds, dates, aggregate_by)), 
    .dots=as.list(agg.by)) 
  groupsInRecord <- n_groups(preds_grp) # Compute the number of groups before filtering
  
  # Remove grouping periods with insufficient non-NA data
  preds_filt <- preds_grp %>%
    filter(if(isTRUE(na.rm)) complete.cases(.) else TRUE) %>%
    filter(n() >= min.count)
  groupsComplete <- n_groups(preds_filt) # Compute the number of groups after filtering
  
  # Compute the means and counts in each group
  agg_preds <- as.data.frame(summarise(
    preds_filt, 
    Value = mean(preds), 
    Count = n()))
  
  # If requested, determine the new units for the conc/flux/fluxrate columns.
  # Other columns (e.g., Period, Duration) are either non-unitted or already
  # have units attached.
  if(attach.units) {
    new_units <- switch(
      format,
      "conc"=metadata@conc.units,
      "flux rate"=metadata@load.rate.units
    )
    retDF <- u(
      agg_preds,
      replace(
        rep(NA, ncol(agg_preds)),
        names(agg_preds) %in% c("Value","SE","CI_lower","CI_upper"),
        new_units))
  } else {
    # Shake off any pre-existing units - e.g., those attached to Duration
    retDF <- v(agg_preds)
  }
  
  # Exclude any un-requested names
  all_names <- names(retDF)
  drop_names <- c(
    if(!agg.cols) agg.by,
    if(!count) c("Count", "Years_Record", "Years_Complete"))
  keep_names <- setdiff(all_names, drop_names)
  retDF <- retDF[ , keep_names] # drops to vector if the only thing in keep_names is the Value column

  # Give the data.frame nice column names
  names(retDF)[1] <- .reSpace(.sentenceCase(names(retDF)[1]), "_")
  names(retDF)[match("Value", names(retDF))] <- .reSpace(.sentenceCase(format), "_")
  
  return(retDF)
}
