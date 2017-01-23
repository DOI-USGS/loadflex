#' Summarize the site and input data
#' 
#' @return data frame of the following information:
#' @export
#' @param metadata metadata, used to access the appropriate columns of data. At 
#'   a minimum, \code{metadata} should correctly specify the date column and the
#'   column indicated by \code{interp.format}.
#' @param fitdat data frame record of constituent+discharge measurements, for
#'   fitting a model
#' @param estdat data frame record of discharge measurements, for making
#'   predictions from a model
#' @importFrom dplyr mutate bind_cols select %>%
#' @examples
#' data(lamprey_nitrate)
#' data(lamprey_discharge)
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH",
#'   basin.area=50, flow.basin.area=65, basin.area.units='ha',
#'   site.id="1073500", custom=list(data.source="USGS NWIS, waterdata.usgs.gov"))
#' sitesum <- summarizeInputs(metadata=md, fitdat=select(lamprey_nitrate, -REGR), 
#'   estdat=lamprey_discharge)
summarizeInputs <- function(metadata, fitdat, estdat) {
  # convert metadata into data.frame and add a statistic or two
  site.info <- 
    as.data.frame(metadata) %>%
    select(-constituent, -flow, -load.rate, -dates, -station) %>%
    select(site.name, site.id, consti.name, everything()) %>%
    mutate(basin.area.ratio.QC = flow.basin.area / basin.area)
  
  # compute date statistcs for both input datasets
  fitdat.stats <- summarizeInput(metadata, fitdat)
  estdat.stats <- summarizeInput(metadata, estdat)
  
  # combine all info into a single data.frame row
  all.info <- data.frame(
    site.info, 
    fitdat=fitdat.stats, 
    estdat=estdat.stats, 
    stringsAsFactors=FALSE)
  
  return(all.info)
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
