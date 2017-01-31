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
#' @importFrom dplyr mutate bind_cols select %>% everything
#' @examples
#' data(lamprey_nitrate)
#' data(lamprey_discharge)
#' md <- metadata(constituent="NO3", flow="DISCHARGE", 
#'   dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
#'   load.rate.units="kg d^-1", site.name="Lamprey River, NH",
#'   basin.area=50, flow.basin.area=65, basin.area.units='ha',
#'   site.id="1073500", custom=list(data.source="USGS NWIS, waterdata.usgs.gov"))
#' sitesum <- summarizeInputs(metadata=md, fitdat=lamprey_nitrate[,1:3], 
#'   estdat=lamprey_discharge)
summarizeInputs <- function(metadata, fitdat, estdat) {
  
  constituent <- flow <- load.rate <- dates <- station <- site.name <- 
    site.id <- consti.name <- flow.basin.area <- basin.area <- '.dplyr.var'
  
  # convert metadata into data.frame and add a statistic or two
  site.info <- 
    as.data.frame(metadata) %>%
    select(-flow, -load.rate, -dates, -station) %>%
    select(site.name, site.id, constituent, consti.name, everything()) %>%
    mutate(basin.area.ratio.QC = flow.basin.area / basin.area)
  
  # compute date statistcs for both input datasets
  fitdat.stats <- summarizeTimeseries(metadata, fitdat)
  estdat.stats <- summarizeTimeseries(metadata, estdat)
  
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
#' @importFrom stats median
#' @keywords internal
summarizeTimeseries <- function(metadata, data) {
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

#' Summarize generated predictions 
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
   match.arg(by, c("total", "annual"))
   site.id <- getInfo(meta, "site.id")
   if(by == "total") {
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    #TODO: what happens with partial years? want to leave them out
    SE <- sqrt((1/nrow(annuals)) * sum(annuals$SE ^ 2))
    multiYear <- data.frame(site.id = site.id, 
                            constituent = getInfo(meta, 'constituent'),
                            model = model.name,
                            multi_year_avg = mean(annuals$Flux_Rate),
                            SE, CI_lower = mean(annuals$Flux_Rate) - 2*SE,
                            CI_upper = mean(annuals$Flux_Rate) + 2*SE,
                            stringsAsFactors = FALSE)
    retDF <- multiYear
  } else if(by == "annual") {
    annuals <- aggregateSolute(preds, metadata = meta, format = "flux rate",
                               agg.by = "water year")
    retDF <- data.frame(site.id = rep(site.id, nrow(annuals)), 
                        constituent = rep(getInfo(meta, 'constituent'), nrow(annuals)),
                        model = rep(model.name, nrow(annuals)),
                        annuals, stringsAsFactors = FALSE)
  }
  return(retDF)
}

## see also summarizeModel in loadModelInterface.R
