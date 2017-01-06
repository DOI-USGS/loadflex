#' Convert loadflex to EGRET object
#'  
#' Convert a loadflex object into an EGRET object for plotting.
#' 
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
#' @param preds.type character specifying if the predictions being used are
#' concentrations ("Conc") or fluxes ("Flux").
#' 
#' @importFrom EGRET as.egret
#' 
convertToEGRET <- function(intdat = NULL, estdat = NULL, preds = NULL, meta = NULL, preds.type = "Conc") {
  
  info_df <- convertToEGRETInfo(meta, preds.type)
  
  # TO DO: automate the conversion factor calculation using the units 
  # given in info_df for param.units. EGRET expects cms.
  # qconvert <- getConversionFactor(info_df)
  qconvert <- 35.314667
  
  daily_df <- convertToEGRETDaily(estdat, meta, preds, preds.type, qconvert)
  
  if(is.null(preds)){
    sample_df <- convertToEGRETSample(intdat, meta, qconvert)
  } else {
    sample_df <- convertToEGRETSample(intdat, meta, qconvert, daily_df)
  }
  
  eList <- as.egret(INFO=info_df, Daily=daily_df, Sample=sample_df)
  return(eList)
}

#' Convert the interpolation data.frame into the EGRET Sample dataframe.
#'
#' @param intdat data.frame of interpolation data
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
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
convertToEGRETSample <- function(intdat, meta, qconvert = 35.314667, dailydat = NULL) {
  if(any(is.null(intdat), is.null(meta))) {
    return(NA)
  }
  
  flow_col <- verify_meta(meta, 'flow')
  constituent <- verify_meta(meta, 'constituent')
  sample_df1 <- intdat %>% 
    rename_("value" = constituent,
            "dateTime" = "DATE")  %>%
    select(dateTime, ConcHigh = value) %>% 
    mutate(ConcLow = ConcHigh, 
           Uncen=as.numeric(ConcHigh == ConcLow)) %>% 
    populateSampleColumns() %>% 
    mutate(dateTime = intdat$DATE,
           Date = as.Date(Date))
  
  flow_data <- flowCorrectionEGRET(flowdat = intdat, 
                                   flow.colname = flow_col,
                                   qconvert = qconvert) %>% 
    select(Date, Q, dateTime)
  
  sample_df <- sample_df1 %>%
    left_join(flow_data, by=c("dateTime","Date"))
  
  if(!is.null(dailydat)){
    subDaily <- select(sample_df, dateTime) %>%
      left_join(select(dailydat, dateTime, SE, yHat), by="dateTime") %>%
      mutate(ConcHat = yHat*exp((SE^2)/2)) 
    
    sample_df <- bind_cols(sample_df, subDaily)
  }
  
  return(sample_df)
}

#' Convert a loadflex metadata object into the EGRET INFO dataframe.
#' 
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
#' @param preds.type character specifying if the predictions being used are
#' concentrations ("Conc") or fluxes ("Flux").
#' 
convertToEGRETInfo <- function(meta, preds.type = 'Conc') {
  if(is.null(meta)) {
    stop("metadata is required to create an EGRET eList")
  }
  
  info_df <- data.frame(shortName=verify_meta(meta, 'station'),
                        paramShortName=verify_meta(meta, c('custom', 'consti.name')),
                        staAbbrev=verify_meta(meta, c('custom', 'sta.abbr')),
                        constitAbbrev=verify_meta(meta, 'constituent'),
                        param.units=verify_meta(meta, paste0(tolower(preds.type), '.units')),
                        stringsAsFactors = FALSE)
  return(info_df)
}

#' Convert estimation and load prediction data into the EGRET Daily data.frame
#' 
#' @param estdat data.frame of estimation data
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
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
  
  stopifnot(preds.type %in% c('Conc', 'Flux'))

  flow_col <- verify_meta(meta, 'flow')
  
  daily_df <- flowCorrectionEGRET(flowdat = estdat, 
                                  flow.colname = flow_col,
                                  qconvert = qconvert)
  
  if(!is.null(preds)){
  
    daily_df <- daily_df %>% 
      left_join(preds, by=c("dateTime" = "date")) %>% 
      rename_(.dots = setNames("fit",paste0(preds.type, "Day"))) %>% 
      rename(SE = se.pred)
  
    if(preds.type == "Conc"){
      daily_df <- mutate(daily_df, FluxDay = ConcDay*daily_df$Q * 86.4)
    } else {
      daily_df <- mutate(daily_df, ConcDay = FluxDay/(daily_df$Q * 86.4))
    }
      
    # TO DO: explore issues with bias
    # ldecicco "In EGRET, it's a little more complicated than just 
    # log(C) (there's various bias correction things going on)"
    daily_df <- daily_df %>% mutate(yHat = log(ConcDay))
  }
  
  return(daily_df)
}

#' Verify metadata object
#'  
#' Verify that the loadflex metadata object has everything it needs.
#' 
#' @param meta loadflex metadata object
#' @param nm character name of the metadata item to check. If it is a 
#' custom name, this would be a character vector with the first name as
#' 'custom' (e.g. nm = c('custom', 'staAbbr'))
#' 
#' @export
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

#' Use EGRET functions to correct flow values
#' 
#' @description Uses the EGRET function populateDaily to convert values 
#' into cubic meters per second.
#' 
#' @param flowdat data frame with discharge values to convert
#' @param flow.colname character string giving the column name that corresponds to flow
#' @param qconvert numeric conversion factor to get flow into cubic meters per second. Default
#' conversion factor is for cubic feet per second.
#' 
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate
#' @importFrom EGRET populateDaily
flowCorrectionEGRET <- function(flowdat, flow.colname, qconvert = 35.314667){
  flowdat_corrected <- flowdat %>% 
    rename_("value" = flow.colname,
            "dateTime" = "DATE") %>% 
    mutate(code = "") %>% 
    populateDaily(qConvert = qconvert, interactive = FALSE) %>%
    mutate(dateTime = flowdat$DATE)
  return(flowdat_corrected)
}
