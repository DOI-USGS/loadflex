#' summarize WQ site information
#'
#' @return data frame of the following information:
#' @export
#' @param site.info data frame containing site information, with column for station id
#' @param constit.df data frame record of constituent measurements
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
summarizeSite <- function(site.info, constit.df) {
  #get existing site metadata?
 
  
  #add metrics that have to be calculated
  if(!is.Date(constit.df$date)) {
    constit.df <- mutate(constit.df, date = as.Date(date))
  }
  
  #will need work if summarizeSites will deal with multiple sites
  #could be useful as a stand-alone function?
  dateStats <- getDateStats(constit.df$date)
  
  site.info <- bind_cols(site.info, dateStats)
  
  return(site.info)
}

#' Get various summary statistics on a date vector
#'
#' @param date.col vector of dates
#' 
#' @return data frame of date statistics, including 
#'
getDateStats <- function(date.col) {
  #is there a function that could be used instead of this?
  start <- min(date.col)
  end <- max(date.col)
  n <- length(date.col)
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
summarizeModel.loadReg2 <- function(model) {
  
}

#' Summarize generated predictions 
#' just take one site for now
#'
#' @param preds data frame input data frame of predicted solutes
#' @param meta loadflex metadata object for the preds data frame
#' @param by character "total" to return average flux over all years, "annual" to return 
#' yearly (water year) averages
#' @param model.name char name of model used to generate predictions
#' @return data frame containing various statistics
#' @export
#'
summarizePreds <- function(preds, meta, by, model.name) {
   station <- getInfo(meta, field = c("station"))
   if(by == "total") {
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    multiYear <- data.frame(station = station, model = model.name,
                            multi_year_avg = mean(annuals$Flux_Rate), stringsAsFactors = FALSE)
    retDF <- multiYear
  }else if(by == "annual") {
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    retDF <- data.frame(station = rep(station, nrow(annuals)), 
                        model = rep(model.name, nrow(annuals)),annuals)
  }
  return(retDF)
}
