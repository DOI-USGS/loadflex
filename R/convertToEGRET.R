#' Convert loadflex to EGRET object
#' 
#' @description Convert a loadflex object into an EGRET object for plotting.
#'   
#' @param data data.frame of data used to fit a model. only required if
#'   load.model is omitted
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions. only required if load.model is
#'   omitted
#' @param meta loadflex metadata object; it must include constituent, flow, 
#'   dates, conc.units, site.id, and consti.name. only required if load.model is
#'   omitted
#' @param preds.type character specifying if the predictions being used are 
#'   concentrations ("Conc") or fluxes ("Flux"). The only permitted value is 
#'   "Conc", and this argument will be leaving soon.
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
#' preds_conc <- predictSolute(no3_lm, "conc", estdat, se.pred=TRUE, date=TRUE)
#' preds_flux <- predictSolute(no3_lm, "flux", estdat, se.pred=TRUE, date=TRUE)
#' loadflex:::convertToEGRET(fitdat, estdat, preds_conc, meta)
convertToEGRET <- function(fitdat = NULL, estdat = NULL, preds = NULL, meta = NULL, preds.type = "Conc") {
  
  # EGRET format is a list of INFO, Daily predictions, and Sample data (possibly
  # combined with predictions for those time points). Collect and combine those
  # pieces.
  
  
  info_df <- convertToEGRETInfo(meta, preds.type)
  
  # EGRET expects cms for all flow values; get the conversion factor
  qconvert <- 1/flowUnitsConversion(verify_meta(meta, 'flow.units'), 'cms')
  
  daily_df <- convertToEGRETDaily(estdat, meta, preds, preds.type, qconvert)
  
  if(is.null(preds)) {
    sample_df <- convertToEGRETSample(data, meta, qconvert)
  } else {
    sample_df <- convertToEGRETSample(data, meta, qconvert, daily_df)
  }
  
  eList <- as.egret(INFO=info_df, Daily=daily_df, Sample=sample_df)
  return(eList)
}

#' Convert the interpolation data.frame into the EGRET Sample dataframe.
#'
#' @param data data.frame of data used to fit a model
#' @param meta loadflex metadata object; it must include constituent,
#' flow, dates, conc.units, site.id, and consti.name
#' @param qconvert numeric conversion factor to get flow into cubic meters per second. Default
#' conversion factor is for cubic feet per second.
#' @param dailydat an EGRET Daily data.frame of flow values
#' 
#' @importFrom dplyr rename_
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom EGRET populateSampleColumns
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_cols
convertToEGRETSample <- function(data, meta, qconvert = 35.314667, dailydat = NULL) {
  if(any(is.null(data), is.null(meta))) {
    return(NA)
  }
  
  dateTime <- value <- ConcHigh <- ConcLow <- Date <- Q <- SE <- yHat <- ConcDay <- '.dplyr.var'
  
  flow_col <- verify_meta(meta, 'flow')
  date_col <- verify_meta(meta, 'dates')
  constituent <- verify_meta(meta, 'constituent')
  sample_df1 <- data %>% 
    rename_("value" = constituent,
            "dateTime" = date_col)  %>%
    select(dateTime, 
           ConcHigh = value) %>% 
    mutate(ConcLow = ConcHigh, 
           Uncen = as.numeric(ConcHigh == ConcLow)) %>% 
    populateSampleColumns() %>% 
    mutate(dateTime = data[[date_col]],
           Date = as.Date(Date))
  
  flow_data <- flowCorrectionEGRET(flowdat = data, 
                                   flow.colname = flow_col,
                                   date.colname = date_col,
                                   qconvert = qconvert) %>% 
    select(Date, Q, dateTime)
  
  sample_df <- sample_df1 %>%
    left_join(flow_data, by=c("dateTime","Date"))
  
  if(!is.null(dailydat)) {
    subDaily <- select(sample_df, dateTime) %>%
      left_join(select(dailydat, dateTime, yHat, SE, ConcHat=ConcDay), by='dateTime')
    # The sample-specific model predictions created by EGRET are actually 
    # leave-one-out estimates where yHat, SE, and ConcHat are the predictions 
    # from a model fit without the given observation. For the time being, we'll 
    # stick to the simpler approach of using the same model for every row in the
    # subDaily dataset. But LOOCV is the uncertainty estimation method for 
    # loadInterp and half of the method for loadComp, so it would be appropriate
    # for at least those models to use an approach like the EGRET approach here
    # someday.
    
    sample_df <- left_join(sample_df, subDaily, by='dateTime')
  }
  
  return(sample_df)
}

#' Convert a loadflex metadata object into the EGRET INFO dataframe.
#' 
#' @param meta loadflex metadata object; it must include site.name, consti.name,
#'   site.id, constituent, and the relevant type of units (conc.units or
#'   load.units, depending on preds.type)
#' @param preds.type character specifying if the predictions being used are 
#'   concentrations ("Conc") or fluxes ("Flux").
#'   
convertToEGRETInfo <- function(meta, preds.type = 'Conc') {
  if(is.null(meta)) {
    stop("metadata is required to create an EGRET eList")
  }
  match.arg(preds.type)
  
  info_df <- data.frame(shortName=verify_meta(meta, 'site.name'),
                        paramShortName=verify_meta(meta, 'consti.name'),
                        staAbbrev=verify_meta(meta, 'site.id'),
                        constitAbbrev=verify_meta(meta, 'constituent'),
                        param.units=verify_meta(meta, 'conc.units'),
                        stringsAsFactors = FALSE)
  return(info_df)
}

#' Convert estimation and load prediction data into the EGRET Daily data.frame
#' 
#' @param estdat data.frame of estimation data
#' @param meta loadflex metadata object; it must include constituent,
#' flow, dates, conc.units, site.id, and consti.name
#' @param preds data.frame of load predictions
#' @param preds.type character specifying if the predictions being used are
#' concentrations ("Conc") or fluxes ("Flux").
#' @param qconvert numeric conversion factor to get flow into cubic meters per second. Default
#' conversion factor is for cubic feet per second.
#' 
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate 
#' @importFrom EGRET populateDaily
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
convertToEGRETDaily <- function(estdat, meta, preds, preds.type = "Conc", qconvert = 35.314667) {
  if(any(is.null(estdat), is.null(preds), is.null(meta))) {
    return(NA)
  }
  
  # from https://github.com/USGS-R/EGRET/blob/0a44aa92c8f473ffd67742c866588d45e3e4d8c9/R/estSurfaces.R#L5-L8:
  # the EGRET surfaces/columns are:
  #   (1) is the estimated log concentration (yHat), 
  #   (2) is the estimated standard error (SE), 
  #   (3) is the estimated concentration (ConcHat). 
  
  stopifnot(preds.type == 'Conc') # We need it to be Conc to work with EGRET. No choice.
  
  daily_df <- flowCorrectionEGRET(flowdat = estdat, 
                                  flow.colname = verify_meta(meta, 'flow'),
                                  date.colname = verify_meta(meta, 'dates'),
                                  qconvert = qconvert)
  
  if(!is.null(preds)) {
    
    fit <- se.pred <- ConcDay <- FluxDay <- '.dplyr.var'
    
    # merge daily_df with preds
    daily_df <- daily_df %>% 
      left_join(preds, by=c("dateTime" = "date")) %>% 
      rename(
        ConcDay = fit,
        SE = se.pred) %>% # in EGRET, the SE always describes the SE of yHat, so this is wrong. see #172
      mutate(
        yHat = log(ConcDay)) # yHat should differ from log(ConcDay) by a model-specific bias correction factor. see #134
    
    # fill in the Flux preds based on the Conc preds
    meta.conv <- updateMetadata(meta, flow='Q', flow.units='cms', load.rate.units='kg d^-1')
    daily_df <- mutate(daily_df, FluxDay = formatPreds(daily_df$ConcDay, 'conc', 'flux', newdata=daily_df, metadata=meta.conv))
  }
  
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
#' @param qconvert numeric conversion factor to get flow into cubic meters per 
#'   second. Default conversion factor is for cubic feet per second.
#'   
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate
#' @importFrom EGRET populateDaily
flowCorrectionEGRET <- function(flowdat, flow.colname, date.colname, qconvert = 35.314667) {
  flowdat_corrected <- flowdat %>% 
    rename_("value" = flow.colname,
            "dateTime" = date.colname) %>% 
    mutate(code = "") %>% 
    populateDaily(qConvert = qconvert, interactive = FALSE) %>%
    mutate(dateTime = flowdat[[date.colname]])
  return(flowdat_corrected)
}
