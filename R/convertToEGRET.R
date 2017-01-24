#' Convert loadflex to EGRET object
#'  
#' @description Convert a loadflex object into an EGRET object for plotting.
#' 
#' @param fitdat data.frame of data used to fit a model
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, dates, conc.units, site.id, and consti.name
#' @param preds.type character specifying if the predictions being used are
#' concentrations ("Conc") or fluxes ("Flux").
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
  
  info_df <- convertToEGRETInfo(meta, preds.type)
  
  # EGRET expects cms.
  qconvert <- 1/flowUnitsConversion(verify_meta(meta, 'flow.units'), 'cms')
  
  daily_df <- convertToEGRETDaily(estdat, meta, preds, preds.type, qconvert)
  
  if(is.null(preds)) {
    sample_df <- convertToEGRETSample(fitdat, meta, qconvert)
  } else {
    sample_df <- convertToEGRETSample(fitdat, meta, qconvert, daily_df)
  }
  
  eList <- as.egret(INFO=info_df, Daily=daily_df, Sample=sample_df)
  return(eList)
}

#' Convert the interpolation data.frame into the EGRET Sample dataframe.
#'
#' @param fitdat data.frame of data used to fit a model
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
convertToEGRETSample <- function(fitdat, meta, qconvert = 35.314667, dailydat = NULL) {
  if(any(is.null(fitdat), is.null(meta))) {
    return(NA)
  }
  
  dateTime <- value <- ConcHigh <- ConcLow <- Date <- Q <- SE <- yHat <- '.dplyr.var'
  
  flow_col <- verify_meta(meta, 'flow')
  date_col <- verify_meta(meta, 'dates')
  constituent <- verify_meta(meta, 'constituent')
  sample_df1 <- fitdat %>% 
    rename_("value" = constituent,
            "dateTime" = date_col)  %>%
    select(dateTime, ConcHigh = value) %>% 
    mutate(ConcLow = ConcHigh, 
           Uncen=as.numeric(ConcHigh == ConcLow)) %>% 
    populateSampleColumns() %>% 
    mutate(dateTime = fitdat[[date_col]],
           Date = as.Date(Date))
  
  flow_data <- flowCorrectionEGRET(flowdat = fitdat, 
                                   flow.colname = flow_col,
                                   date.colname = date_col,
                                   qconvert = qconvert) %>% 
    select(Date, Q, dateTime)
  
  sample_df <- sample_df1 %>%
    left_join(flow_data, by=c("dateTime","Date"))
  
  if(!is.null(dailydat)) {
    subDaily <- select(sample_df, dateTime) %>%
      left_join(select(dailydat, dateTime, SE, yHat), by="dateTime") %>%
      # the following line suggests to me that SE is in log space in EGRET, but
      # in convertToEGRETDaily we're currently passing in the retransformed SE
      # for the predictions. One or the other of these needs to get fixed.
      mutate(ConcHat = exp(yHat)*exp((SE^2)/2)) 
    
    sample_df <- bind_cols(sample_df, subDaily)
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
  
  info_df <- data.frame(shortName=verify_meta(meta, 'site.name'),
                        paramShortName=verify_meta(meta, 'consti.name'),
                        staAbbrev=verify_meta(meta, 'site.id'),
                        constitAbbrev=verify_meta(meta, 'constituent'),
                        param.units=verify_meta(meta, c('Conc'='conc.units', 'Flux'='load.rate.units')[[preds.type]]),
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
  
  #stopifnot(preds.type %in% c('Conc', 'Flux'))
  stopifnot(preds.type=='Conc') # we need it to be Conc so we can get the SE for conc below
  
  daily_df <- flowCorrectionEGRET(flowdat = estdat, 
                                  flow.colname = verify_meta(meta, 'flow'),
                                  date.colname = verify_meta(meta, 'dates'),
                                  qconvert = qconvert)
  
  if(!is.null(preds)) {
    
    se.pred <- ConcDay <- FluxDay <- '.dplyr.var'
    
    # merge daily_df with preds
    daily_df <- daily_df %>% 
      left_join(preds, by=c("dateTime" = "date")) %>% 
      rename_(.dots = setNames("fit",paste0(preds.type, "Day"))) %>% 
      rename(SE = se.pred) # in EGRET, the SE always describes the SE in ConcDay. So we should be converting it here if preds.type="Flux"
    if(preds.type=='Flux') warning("convertToEGRETDaily is probably not handling preds.type=='Flux' correctly yet")
    
    # fill in whichever preds weren't given
    if(preds.type == "Conc") {
      meta.conv <- updateMetadata(meta, flow='Q', flow.units='cms', load.rate.units='kg d^-1')
      daily_df <- mutate(daily_df, FluxDay = formatPreds(daily_df$ConcDay, 'conc', 'flux', newdata=daily_df, metadata=meta.conv))
      # daily_df <- mutate(daily_df, FluxDay = ConcDay*daily_df$Q * 86.4)
    } else {
      meta.conv <- updateMetadata(meta, flow='Q', flow.units='cms', conc.units='mg L^-1')
      daily_df <- mutate(daily_df, ConcDay = formatPreds(daily_df$FluxDay, 'flux', 'conc', newdata=daily_df, metadata=meta.conv))
      # daily_df <- mutate(daily_df, ConcDay = FluxDay/(daily_df$Q * 86.4))
    }
    
    # TO DO: explore issues with bias. See GitHub issue #134
    daily_df <- daily_df %>% mutate(yHat = log(ConcDay))
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
