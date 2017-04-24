#' Summarize the site and input data
#' 
#' @md
#' @param metadata metadata, used to access the appropriate columns of data. At 
#'   a minimum, `metadata` should correctly specify the date column and the 
#'   column indicated by `interp.format`.
#' @param fitdat data frame of constituent+discharge measurements, for fitting a
#'   model
#' @param estdat data frame of discharge measurements, for making predictions 
#'   (estimations) from a model
#' @return Returns a 1-row data frame with the following columns:
#'   
#'   * `site.name` - the long name of the site, as in [metadata()]
#'   
#'   * `site.id` - the unique identifier of the site, as in [metadata()]
#'   
#'   * `constituent` - the unique identifier of the constituent, as in 
#'   [metadata()]
#'   
#'   * `consti.name` - the long name of the constituent, as in [metadata()]
#'   
#'   * `conc.units` - the units of concentration data, as in [metadata()]
#'   
#'   * `flow.units` - the units of discharge data, as in [metadata()]
#'   
#'   * `load.units` - the units of load estimates, as in [metadata()]
#'   
#'   * `load.rate.units` - the units of load rate units, as in [metadata()]
#'   
#'   * `lat` - the decimal latitude of the station where concentration (and 
#'   possibly also discharge) was measured, as in [metadata()]
#'   
#'   * `lon` - the decimal longitude of the station where concentration (and 
#'   possibly also discharge) was measured, as in [metadata()]
#'   
#'   * `basin.area` - the area of the drainage basin contributing water to the 
#'   site where concentrations were measured, as in [metadata()]
#'   
#'   * `flow.site.name` - the long name of the station where flow was monitored,
#'   as in [metadata()]
#'   
#'   * `flow.site.id` - the unique identifier of the station where flow was 
#'   monitored, as in [metadata()]
#'   
#'   * `flow.lat` - the decimal latitude of the station where discharge was 
#'   measured, as in [metadata()]
#'   
#'   * `flow.lon` - the decimal longitude of the station where discharge was 
#'   measured, as in [metadata()]
#'   
#'   * `flow.basin.area` - the area of the drainage basin contributing water to 
#'   the site where discharge was measured, as in [metadata()]
#'   
#'   * `basin.area.units` - the units of the values in `basin.area` and 
#'   `flow.basin.area` (same for both), as in [metadata()]
#'   
#'   * `[custom]` - if the `custom` slot of `metadata` is a 1-row data.frame or 
#'   a list of length-1 elements, the columns of that data.frame are included in
#'   this summary
#'   
#'   * `basin.area.ratio.QC` - the ratio of `flow.basin.area` to `basin.area`
#'   
#'   * `fitdat.start` - the date of the first observation in the model fitting 
#'   data
#'   
#'   * `fitdat.end` - the date/time of the last observation in the model fitting
#'   data
#'   
#'   * `fitdat.num.total` - the total number of observations in the model 
#'   fitting data
#'   
#'   * `fitdat.num.incomplete` - the number of NA observations in the model 
#'   fitting data
#'   
#'   * `fitdat.num.censored` - this field is currently a placeholder (will 
#'   always be NA) for the number of censored observations in the model fitting 
#'   data
#'   
#'   * `fitdat.min.gap.days` - the length in days of the smallest gap between 
#'   any two successive observations in the model fitting data
#'   
#'   * `fitdat.max.gap.days` - the length in days of the largest gap between any
#'   two successive observations in the model fitting data
#'   
#'   * `fitdat.median.gap.days` - the length in days of the median gap between 
#'   successive observations in the model fitting data
#'   
#'   * `estdat.start` - the date of the first observation in the estimation data
#'   
#'   * `estdat.end` - the date/time of the last observation in the estimation 
#'   data
#'   
#'   * `estdat.num.total` - the total number of observations in the estimation 
#'   data
#'   
#'   * `estdat.num.incomplete` - the number of incomplete (NA) observations in 
#'   the estimation data
#'   
#'   * `estdat.min.gap.days` - the length in days of the smallest gap between 
#'   any two successive observations in the estimation data
#'   
#'   * `estdat.max.gap.days` - the length in days of the largest gap between any
#'   two successive observations in the estimation data
#'   
#'   * `estdat.median.gap.days` - the length in days of the median gap between 
#'   successive observations in the estimation data
#'   
#' @importFrom dplyr mutate bind_cols select %>% everything
#' @export
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
    site.id <- consti.name <- flow.basin.area <- basin.area <- num.censored <- 
    '.dplyr.var'
  
  # convert metadata into data.frame and add a statistic or two
  site.info <- 
    as.data.frame(metadata) %>%
    select(-flow, -load.rate, -dates, -station) %>%
    select(site.name, site.id, constituent, consti.name, everything()) %>%
    mutate(basin.area.ratio.QC = flow.basin.area / basin.area)
  
  # compute date statistcs for both input datasets
  fitdat.stats <- summarizeTimeseries(metadata, fitdat)
  estdat.stats <- summarizeTimeseries(metadata, estdat) %>%
    select(-num.censored)
  
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
  date.col <- getInfo(metadata, 'date', TRUE)
  ccdata <- data[complete.cases(data), ]
  
  input.info <- data.frame(
    start = min(ccdata[[date.col]]), # true start (first non-NA)
    end = max(ccdata[[date.col]]), # true end (last non-NA)
    num.total = nrow(data),
    num.incomplete = nrow(data) - nrow(ccdata),
    num.censored = NA, # placeholder; need to decide how we're storing censored data first. applies to conc only, not Q
    min.gap.days = min(as.numeric(diff(ccdata[[date.col]]), units='days')),
    max.gap.days = max(as.numeric(diff(ccdata[[date.col]]), units='days')),
    median.gap.days = median(as.numeric(diff(ccdata[[date.col]]), units='days')),
    stringsAsFactors = FALSE)
  
  return(input.info)
}
