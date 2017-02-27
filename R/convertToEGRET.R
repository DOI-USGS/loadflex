#' Convert loadflex to EGRET object
#' 
#' @description Convert a loadflex object into an EGRET object for plotting.
#'   
#' @param load.model a load model (loadReg2, loadComp, loadInterp, loadLm, etc.)
#'   whose data and predictions are to be converted to EGRET format
#' @param data data.frame of data used to fit a model. only required if 
#'   load.model is omitted
#' @param newdata data.frame of data used to generate predictions from an
#'   already-fitted model
#' @param meta loadflex metadata object; it must include constituent, flow, 
#'   dates, conc.units, site.id, and consti.name. only required if load.model is
#'   omitted
#' @importFrom EGRET as.egret
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
#' loadflex:::convertToEGRET(data=fitdat, newdata=estdat, meta=meta)
convertToEGRET <- function(load.model = NULL, data = NULL, newdata = NULL, meta = NULL) {
  
  # Prepare inputs
  if(!is.null(load.model)) {
    if(!is.null(data)) warning('data arg will be overridden by load.model')
    if(!is.null(meta)) warning('meta arg will be overridden by load.model')
    data <- getFittingData(load.model)
    meta <- getMetadata(load.model)
  }
  
  # EGRET format is a list of INFO, Daily predictions, and Sample data (possibly
  # with predictions for those time points). Collect the pieces.
  info_df <- convertToEGRETInfo(meta)
  daily_df <- convertToEGRETDaily(load.model, newdata, meta)
  sample_df <- convertToEGRETSample(data, meta, dailydat=if(is.null(load.model)) NULL else daily_df)
  
  # Combine and return the pieces
  eList <- as.egret(INFO=info_df, Daily=daily_df, Sample=sample_df)
  return(eList)
}

#' Convert the interpolation data.frame into the EGRET Sample dataframe.
#' 
#' @inheritParams convertToEGRET
#' @param dailydat an EGRET Daily data.frame of flow values
#' 
#' @importFrom dplyr rename_
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom EGRET populateSampleColumns
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_cols
convertToEGRETSample <- function(data = NULL, meta = NULL, dailydat = NULL) {
  if(any(is.null(data), is.null(meta))) {
    return(NA)
  }
  
  dateTime <- value <- ConcHigh <- ConcLow <- Date <- Q <- SE <- yHat <- ConcDay <- '.dplyr.var'
  
  # Format the sample info
  date_col <- verify_meta(meta, 'dates')
  sample_df1 <- data %>% 
    rename_(
      value = verify_meta(meta, 'constituent'),
      dateTime = date_col)  %>%
    select(
      dateTime, 
      ConcHigh = value) %>% 
    mutate(
      ConcLow = ConcHigh, 
      Uncen = as.numeric(ConcHigh == ConcLow)) %>% 
    populateSampleColumns() %>% 
    mutate(
      dateTime = data[[date_col]],
      Date = as.Date(Date))
  
  # Format the flow info
  flow_data <- data %>%
    flowCorrectionEGRET(
      flow.colname = verify_meta(meta, 'flow'),
      date.colname = date_col,
      flow.units = verify_meta(meta, 'flow.units')) %>% 
    select(Date, Q, dateTime)
  
  # Combine the sample and flow info
  sample_df <- sample_df1 %>%
    left_join(flow_data, by=c('dateTime', 'Date'))
  
  # Add in predictions if available. The sample-specific model predictions
  # created by EGRET are actually leave-one-out estimates where yHat, SE, and
  # ConcHat are the predictions from a model fit without the given observation.
  # For the time being, we'll stick to the simpler approach of using the same
  # model for every row in the subDaily dataset. But LOOCV is the uncertainty
  # estimation method for loadInterp and half of the method for loadComp, so it
  # would be appropriate for at least those models to use an approach like the
  # EGRET approach here someday.
  if(!is.null(dailydat)) {
    subDaily <- select(sample_df, dateTime) %>%
      left_join(select(dailydat, dateTime, yHat, SE, ConcHat=ConcDay), by='dateTime')
    
    sample_df <- left_join(sample_df, subDaily, by='dateTime')
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
    shortName = verify_meta(meta, 'site.name'),
    paramShortName = verify_meta(meta, 'consti.name'),
    staAbbrev = verify_meta(meta, 'site.id'),
    constitAbbrev = verify_meta(meta, 'constituent'),
    param.units = verify_meta(meta, 'conc.units'),
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
convertToEGRETDaily <- function(load.model = NULL, newdata, meta = NULL) {

  # Use meta from load.model if available, and note conflicts
  if(!is.null(meta) && !is.null(load.model)) {
    warning('meta argument will be ignored; metadata from load.model will be used instead')
  } else if(is.null(meta)) {
    meta <- getMetadata(load.model)
  }
  
  # Prepare a data.frame of flow information
  daily_df <- flowCorrectionEGRET(
    flowdat = newdata,
    flow.colname = verify_meta(meta, 'flow'),
    date.colname = verify_meta(meta, 'dates'),
    flow.units = verify_meta(meta, 'flow.units'))
  
  # Return now if we can't add predictions
  if(missing(load.model)) {
    return(daily_df)
  }
  
  # Generate concentration predictions in both linear and log space
  preds_lin <- predictSolute(load.model, 'conc', newdata=newdata, date=TRUE, lin.or.log='linear')
  preds_log <- predictSolute(load.model, 'conc', newdata=newdata, se.pred=TRUE, date=TRUE, lin.or.log='log')
    
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

#' Return a value from metadata or throw error if invalid request
#' 
#' @description Try to get a value of name \code{nm} from the regular or custom
#'   slots in a metadata object. If the named value is unavailable or "", throw
#'   an error.
#'   
#' @param meta loadflex metadata object
#' @param nm character name of the metadata item to check. If it is a custom 
#'   name, this would be a character vector with the first name as 'custom' 
#'   (e.g. nm = c('custom', 'staAbbr'))
#'   
#' @keywords internal
verify_meta <- function(meta, nm) {
  
  if("custom" %in% nm) {
    meta_value <- loadflex::getInfo(meta, nm[1])
    meta_value <- meta_value[[nm[2]]]
  } else {
    meta_value <- loadflex::getInfo(meta, nm)
  }
  
  if(nchar(meta_value) == 0) {
    stop(paste0("metadata item `", nm, "` must exist"))
  }
  
  return(meta_value)
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
#'  
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate
#' @importFrom EGRET populateDaily
flowCorrectionEGRET <- function(flowdat, flow.colname, date.colname, flow.units) {
  
  # Get the conversion factor. EGRET expects cms for all flow values
  qconvert <- 1/flowUnitsConversion(flow.units, 'cms')
  
  # Convert to EGRET format with many columns describing flow
  flowdat_corrected <- flowdat %>% 
    rename_("value" = flow.colname,
            "dateTime" = date.colname) %>% 
    mutate(code = "") %>% 
    populateDaily(qConvert = qconvert, interactive = FALSE) %>%
    mutate(dateTime = flowdat[[date.colname]])
  
  return(flowdat_corrected)
}
