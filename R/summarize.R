#' summarize WQ site information
#'
#' @return data frame of the following information:
#' @export
#' @param sites character ID(s) of sites to summarize
#' @param siteInfo data frame containing site information
#' @param nutriDF data frame record of nutrient measurements
#' @importFrom dplyr filter 
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
#' @examples
summarizeSites <- function(sites, siteInfo, nutriDF){
  #get existing site metadata
  summaryDF <- filter(siteInfo, CODIGO_ESTACAO %in% sites)
  
  #add metrics that have to be calculated
  if(!is.Date(nutriDF$date)){
    nutriDF <- mutate(nutriDF, date = as.Date(date))
  }
  
  #will need work if summarizeSites will deal with multiple sites
  #could be useful as a stand-alone function?
  dateStats <- getDateStats(nutriDF$date)
  
  summaryDF <- bind_cols(summaryDF, dateStats)
  
  return(summaryDF)
}

#' Get various summary statistics on a date vector
#'
#' @param dateCol vector of dates
#' 
#' @return data frame of date statistics, including 
#'
#' @examples
getDateStats <- function(dateCol){
  #is there a function that could be used instead of this?
  start <- min(dateCol)
  end <- max(dateCol)
  n <- length(dateCol)
  statDF <- data.frame(start, end, n, stringsAsFactors = FALSE)
  return(statDF)
}

#' Extract various model summary statistics
#' 
#' @param model the input model object; will extend to several types
#' @export
summarizeModel <- function(model) UseMethod("summarizeModel")

#' stats for rloadest loadReg2 model
#' @rdname summarizeModel
#' 
summarizeModel.loadReg2 <- function(model){
  
}

#' Summarize generated predictions 
#' just take one site for now
#'
#' @param pred data frame input data frame of predicted solutes
#'
#' @return data frame containing various statistics
#' @export
#'
#' @examples
summarizePreds <- function(preds, meta, by, modelName){
   station <- getInfo(siteMeta, field = c("station"))
   if(by == "total"){
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    multiYear <- data.frame(CODIGO_ESTACAO = station, model = modelName,
                            multi_year_avg = mean(annuals$Flux_Rate), stringsAsFactors = FALSE)
    retDF <- multiYear
  }else if(by == "annual"){
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    retDF <- data.frame(CODIGO_ESTACAO = rep(station, nrow(annuals)), 
                        model = rep(modelName, nrow(annuals)),annuals)
  }
  return(retDF)
}
