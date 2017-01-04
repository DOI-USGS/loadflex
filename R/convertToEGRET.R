#' Convert loadflex to EGRET object
#'  
#' Convert a loadflex object into an EGRET object for plotting.
#' 
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr)
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr rename_
#' @importFrom dplyr mutate
#' @importFrom lubridate decimal_date
#' @importFrom EGRET as.egret
#' 
#' 
convertToEGRET <- function(intdat, estdat, preds, meta) {
  
  flow_col <- verify_meta(meta, 'flow')
  info_df <- data.frame(shortName=verify_meta(meta, 'station'),
                        paramShortName='nitrate',
                        staAbbrev=verify_meta(meta, c('custom', 'sta.abbr')),
                        constitAbbrev=verify_meta(meta, 'constituent'),
                        param.units=verify_meta(meta, 'conc.units'),
                        stringsAsFactors = F)
  
  discharge_df <- estdat %>% 
    left_join(preds, by=c("DATE" = "date")) %>% 
    rename_(.dots = setNames(c("DATE", flow_col, "fit"),
                            c("Date", "Q", "ConcDay"))) %>% 
    mutate(LogQ=log(Q))
  
  sample_df <- intdat %>% 
    rename_(.dots=setNames(c("DATE", info_df$constitAbbrev, flow_col), 
                           c("Date", "ConcLow", "Q")))  %>% 
    mutate(ConcHigh=ConcLow, 
           ConcAve=mean(c(ConcLow, ConcHigh)), 
           Uncen=rep(1, nrow(intdat)), 
           LogQ=log(Q),
           DecYear=lubridate::decimal_date(Date))
  
  eList <- as.egret(INFO=info_df, Daily=discharge_df, Sample=sample_df)
  return(eList)
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

  if("custom" %in% nm){
    meta_value <- loadflex::getInfo(meta, nm[1])
    meta_value <- meta_value[[nm[2]]]
  } else {
    meta_value <- loadflex::getInfo(meta, nm)
  }
  
  if(nchar(meta_value) == 0){
    stop(paste0("metadata item `", nm, "` must exist"))
  }
  
  return(meta_value)
}
