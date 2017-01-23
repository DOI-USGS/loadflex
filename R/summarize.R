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

#' Get summary statistics for a single input data.frame
#' 
#' @param metadata object of class metadata, describing the site and data
#' @param data data.frame of input data, either for model fitting (concentration
#'   and discharge) or prediction (discharge only)
#' @return data frame of statistics about the input data
#' @keywords internal
summarizeInput <- function(metadata, data) {
  date.col <- getInfo(metadata, 'date')
  ccdata <- data[complete.cases(data), ]
  
  input.info <- data.frame(
    start = min(ccdata[[date.col]]), # true start (first non-NA)
    end = max(ccdata[[date.col]]), # true end (last non-NA)
    num.total = nrow(data),
    num.incomplete = nrow(data) - nrow(ccdata),
    num.censored = 'TBD', # placeholder; need to decide how we're storing censored data first
    min.gap.days = min(as.numeric(diff(ccdata[[date.col]]), units='days')),
    max.gap.days = max(as.numeric(diff(ccdata[[date.col]]), units='days')),
    median.gap.days = median(as.numeric(diff(ccdata[[date.col]]), units='days')),
    stringsAsFactors = FALSE)
  
  return(input.info)
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
    #TODO: what happens with partial years? want to leave them out
    
    multiYear <- data.frame(station = station, model = model.name,
                            multi_year_avg = mean(annuals$Flux_Rate), stringsAsFactors = FALSE)
    retDF <- multiYear
  } else if(by == "annual") {
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    retDF <- data.frame(station = rep(station, nrow(annuals)), 
                        model = rep(model.name, nrow(annuals)),annuals)
  }
  return(retDF)
}
