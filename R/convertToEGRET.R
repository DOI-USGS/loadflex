#' Convert loadflex to EGRET object
#' 
#' @description Convert a loadflex object into an EGRET object for plotting.
#'   
#' @param load.model a load model (loadReg2, loadComp, loadInterp, loadLm, etc.)
#'   whose data and predictions are to be converted to EGRET format
#' @param newdata data.frame of data used to generate predictions from an
#'   already-fitted model
#' @param data data.frame of data used to fit a model. only required if 
#'   load.model is omitted
#' @param meta loadflex metadata object; it must include constituent, flow, 
#'   dates, conc.units, site.id, and consti.name. only required if load.model is
#'   omitted
#' @importFrom EGRET as.egret
#' @export
#' @examples
#' data(lamprey_nitrate)
#' fitdat <- lamprey_nitrate
#' data(lamprey_discharge)
#' estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT")) 
#' estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day
#' meta <- metadata(constituent="NO3", flow="DISCHARGE", dates="DATE", 
#'   conc.units="mg L^-1", flow.units="cfs", load.units="kg", load.rate.units="kg d^-1", 
#'   site.name="Lamprey River, NH", site.id='NWIS 01073500', consti.name = "nitrate")
#' no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
#'   data=fitdat, metadata=meta, retrans=exp)
#' minimalEGRET <- loadflex:::convertToEGRET(data=fitdat, meta=meta)
#' maximalEGRET <- loadflex:::convertToEGRET(load.model=no3_lm, newdata=estdat)
convertToEGRET <- function(load.model = NULL, newdata = NULL, data = NULL, meta = NULL) {
  
  # Reconcile load.model, data, and meta
  if(!is.null(load.model)) {
    if(!is.null(data)) warning('data arg will be overridden by load.model')
    if(!is.null(meta)) warning('meta arg will be overridden by load.model')
    data <- getFittingData(load.model)
    meta <- getMetadata(load.model)
  }
  
  # EGRET format is a list of INFO, Daily predictions, and Sample data (possibly
  # with predictions for those time points). Collect the pieces.
  info_df <- convertToEGRETInfo(meta)
  daily_df <- convertToEGRETDaily(newdata, load.model, meta=if(is.null(load.model)) meta else NULL)
  sample_df <- convertToEGRETSample(data, meta, daily_df)
  
  # Combine and return the pieces
  eList <- as.egret(INFO=info_df, Daily=daily_df, Sample=sample_df)
  return(eList)
}

#' Convert the interpolation data.frame into the EGRET Sample dataframe.
#' 
#' @inheritParams convertToEGRET
#' @param dailydat an EGRET Daily data.frame of flow and prediction values
#' 
#' @importFrom dplyr rename_ select select_ mutate left_join bind_cols
#' @importFrom EGRET populateSampleColumns
#' @importFrom methods S3Part
convertToEGRETSample <- function(data = NULL, meta = NULL, dailydat = NULL) {
  if(any(is.null(data), is.null(meta))) {
    return(NA)
  }
  
  dateTime <- value <- ConcHigh <- ConcLow <- Date <- Q <- SE <- yHat <- ConcDay <- '.dplyr.var'
  
  # Format the sample info
  date_col <- getInfo(meta, 'dates', TRUE)
  # For now, allow value column to be smwrQW and convert here to ConcLow and 
  # ConcHigh. Later we'll get fancier about censored data throughout loadflex.
  smwrQW_cols <- names(which(lapply(data, function(col) attr(class(col), 'package')) == 'smwrQW'))
  const_col <- getInfo(meta, 'constituent', TRUE)
  if(const_col %in% smwrQW_cols) {
    if(class(data[[const_col]]) != 'lcens') {
      stop("we only recognize lcens censoring from smwrQW for now. please submit an issue if you want more")
    }
    vals <- S3Part(data[[const_col]], strictS3=TRUE, S3Class='matrix')[,'values']
    censored <- attr(data[[const_col]], 'censor.codes')
    sample_data <- data %>% 
      select_(dateTime = date_col) %>%
      mutate( 
        ConcLow  = ifelse(censored, NA, vals),
        ConcHigh = vals)
  } else {
    sample_data <- data %>% 
      select_(
        dateTime = date_col, 
        ConcLow  = const_col) %>%
      mutate(
        ConcHigh = ConcLow)
  }
  # Finish formatting the sample info
  sample_data <- sample_data %>% # use transform b/c mutate breaks with smwrQW columns
    mutate(Uncen = as.numeric(!is.na(ConcLow) & !is.na(ConcHigh) & ConcHigh == ConcLow)) %>% 
    populateSampleColumns() %>% 
    mutate(
      dateTime = data[[date_col]],
      Date = data[[date_col]])
  
  # Format the flow info
  flow_col <- getInfo(meta, 'flow', TRUE)
  flow_data <- data %>%
    select_(date_col, flow_col) %>%
    expandFlowForEGRET(
      flow.colname = flow_col,
      date.colname = date_col,
      flow.units = getUnits(meta, 'flow', 'EGRET')) %>% 
    select(Date, Q, dateTime)
  
  # Combine the sample and flow info
  sample_df <- sample_data %>%
    left_join(flow_data, by=c('dateTime', 'Date'))
  
  # Add in predictions if available. The sample-specific model predictions
  # created by EGRET are actually leave-one-out estimates where yHat, SE, and
  # ConcHat are the predictions from a model fit without the given observation.
  # For the time being, we'll stick to the simpler approach of using the same
  # model for every row in the subDaily dataset. But LOOCV is the uncertainty
  # estimation method for loadInterp and half of the method for loadComp, so it
  # would be appropriate for at least those models to use an approach like the
  # EGRET approach here someday.
  if(!is.null(dailydat) && all(c('yHat','SE','ConcDay') %in% names(dailydat))) {
    sample_df <- left_join(
      sample_df, 
      select(dailydat, dateTime, yHat, SE, ConcHat=ConcDay), 
      by='dateTime')
  }
  
  return(sample_df)
}

#' Convert a loadflex metadata object into the EGRET INFO dataframe.
#' 
#' @param meta loadflex metadata object; it must include site.name, consti.name,
#'   site.id, constituent, and conc.units
convertToEGRETInfo <- function(meta) {
  if(is.null(meta)) {
    stop("metadata is required to create an EGRET eList")
  }
  
  info_df <- data.frame(
    shortName = getInfo(meta, 'site.name', TRUE), # do we need to require these to be non-empty?
    paramShortName = getInfo(meta, 'consti.name', TRUE),
    staAbbrev = getInfo(meta, 'site.id', TRUE),
    constitAbbrev = getInfo(meta, 'constituent', TRUE),
    param.units = getUnits(meta, 'conc', 'EGRET'),
    stringsAsFactors = FALSE)
  
  return(info_df)
}

#' Convert estimation and load prediction data into the EGRET Daily data.frame
#' 
#' @inheritParams convertToEGRET
#' @param meta loadflex metadata object; it must include constituent,
#' flow, dates, conc.units, site.id, and consti.name
#' 
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate 
#' @importFrom EGRET populateDaily
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
convertToEGRETDaily <- function(newdata, load.model = NULL, meta = NULL) {
  
  # return NA if daily data.frame can't be created
  if(missing(newdata) || is.null(newdata)) {
    return(NA)
  }
  
  # Use meta from load.model if available, and note conflicts
  if(!is.null(meta) && !is.null(load.model)) {
    warning('meta argument will be ignored; metadata from load.model will be used instead')
  } else if(is.null(meta)) {
    meta <- getMetadata(load.model)
  }
  
  # Prepare a data.frame of flow information
  daily_df <- expandFlowForEGRET(
    flowdat = newdata,
    flow.colname = getInfo(meta, 'flow', TRUE),
    date.colname = getInfo(meta, 'dates', TRUE),
    flow.units = getUnits(meta, 'flow', 'EGRET'))
  
  # Return now if we can't add predictions
  if(is.null(load.model)) {
    return(daily_df)
  }
  
  # Generate concentration predictions in both linear and log space
  preds_lin <- predictSolute(load.model, 'conc', newdata=newdata, date=TRUE, lin.or.log='linear')
  preds_log <- tryCatch(
    predictSolute(load.model, 'conc', newdata=newdata, se.pred=TRUE, date=TRUE, lin.or.log='log'),
    error=function(e) predictSolute(load.model, 'conc', newdata=newdata, se.pred=FALSE, date=TRUE, lin.or.log='log') %>%
      mutate(se.pred=NA)
  )
    
  # Merge daily_df with preds. from
  # https://github.com/USGS-R/EGRET/blob/0a44aa92c8f473ffd67742c866588d45e3e4d8c9/R/estSurfaces.R#L5-L8:
  # the EGRET surfaces/columns are:
  #   (1) is the estimated log concentration (yHat), 
  #   (2) is the estimated standard error (SE), 
  #   (3) is the estimated concentration (ConcHat). 
  fit <- se.pred <- '.dplyr.var'
  daily_df <- daily_df %>%
    left_join(select(preds_log, date, yHat = fit, SE = se.pred), by=c("dateTime" = "date")) %>%
    left_join(select(preds_lin, date, ConcDay = fit), by=c("dateTime" = "date"))
  
  # Fill in the Flux preds based on the Conc preds
  meta.conv <- updateMetadata(meta, flow='Q', flow.units='cms', load.rate.units='kg d^-1')
  daily_df <- mutate(
    daily_df,
    FluxDay = formatPreds(daily_df$ConcDay, 'conc', 'flux', newdata=daily_df, metadata=meta.conv))
  
  return(daily_df)
}

#' Convert a date and discharge data.frame into EGRET format
#' 
#' @description Use EGRET functions to convert a data.frame of date and 
#'   discharge columns into "the basic Daily data frame used in WRTDS", i.e., a 
#'   13-column data.frame with parsed datetimes, log(Q), and 7-day and 30-day 
#'   smoothed Q, plus a column for dateTime. See 
#'   \code{\link[EGRET]{populateDaily}}
#'   
#' @param flowdat data frame with discharge values to convert
#' @param flow.colname character string giving the column name that corresponds 
#'   to flow
#' @param date.colname character string giving the column name that corresponds 
#'   to dates
#' @param flow.units character string giving the current units of flow in
#'   flowdat
#'   
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate
#' @importFrom EGRET populateDaily
expandFlowForEGRET <- function(flowdat, flow.colname, date.colname, flow.units) {
  
  # Get the conversion factor. EGRET expects cms for all flow values
  qconvert <- 1/convertUnits(flow.units, 'cms')
  
  # Convert to EGRET format with many columns describing flow
  flowdat_corrected <- flowdat %>% 
    rename_("value" = flow.colname,
            "dateTime" = date.colname) %>% 
    mutate(code = "") %>% 
    populateDaily(qConvert = qconvert, interactive = FALSE) %>%
    mutate(dateTime = flowdat[[date.colname]])
  
  return(flowdat_corrected)
}
