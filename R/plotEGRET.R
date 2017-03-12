#' Create an EGRET-style plot
#'  
#' @description Use loadflex data frames, but create an EGRET plot.
#' 
#' @param plot.name the name of the plot the user wants to create. See 
#' Details for current options. For now, only one allowed at time.
#' @inheritParams convertToEGRET
#' @param moreTitle additional text to include in the fluxBiasMulti plot 
#' title. The EGRET default is "WRTDS", so this changes the default to "loadflex".
#' @param plotFlowNorm logical indicating whether or not to plot the normalized flow
#' lines. This defaults to FALSE, which overrides the EGRET default TRUE. Applicable 
#' in plotFluxHist and plotConcHist.
#' @param ... additional arguments to pass to the plot
#'
#' @details EGRET plots that require \code{data, meta}:
#' \itemize{
#'   \item boxConcMonth
#'   \item plotConcTime
#'   \item plotConcQ
#'   \item plotFluxQ
#' }
#' EGRET plots that require \code{data, newdata, preds, meta}:
#' \itemize{
#'   \item boxQTwice
#'   \item multiPlotDataOverview
#'   \item plotConcTimeDaily
#'   \item plotFluxTimeDaily
#'   \item plotConcPred 
#'   \item plotFluxPred 
#'   \item plotResidPred
#'   \item plotResidQ  
#'   \item plotResidTime
#'   \item boxResidMonth
#'   \item boxConcThree 
#'   \item plotConcHist 
#'   \item plotFluxHist
#'   \item fluxBiasMulti 
#' }
#'
#' @importFrom EGRET boxConcMonth 
#' @importFrom EGRET plotConcTime 
#' @importFrom EGRET plotConcQ 
#' @importFrom EGRET plotFluxQ 
#' @importFrom EGRET boxQTwice 
#' @importFrom EGRET multiPlotDataOverview 
#' @importFrom EGRET plotConcTimeDaily 
#' @importFrom EGRET plotFluxTimeDaily 
#' @importFrom EGRET plotConcPred  
#' @importFrom EGRET plotFluxPred  
#' @importFrom EGRET plotResidPred 
#' @importFrom EGRET plotResidQ   
#' @importFrom EGRET plotResidTime 
#' @importFrom EGRET boxResidMonth 
#' @importFrom EGRET boxConcThree 
#' @importFrom EGRET plotConcHist  
#' @importFrom EGRET plotFluxHist
#' @importFrom EGRET fluxBiasMulti
#'
#' @export
#' 
#' @examples 
#' 
#' # Load necessary data + create the appropriate metadata
#' data(lamprey_nitrate) # interpolation data (grab sample obs)
#' fitdat <- lamprey_nitrate
#' 
#' data("lamprey_discharge")
#' estdat <- lamprey_discharge # estimation data (15 min interval)
#' estdat <- subset(estdat, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT")) 
#' estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day
#' 
#' meta <- metadata(
#'   constituent="NO3", 
#'   flow="DISCHARGE",
#'   dates="DATE", 
#'   conc.units="mg L^-1", 
#'   flow.units="cfs", 
#'   load.units="kg",
#'   load.rate.units="kg d^-1", 
#'   site.name="Lamprey River, NH",
#'   site.id="01073500",
#'   consti.name="Nitrate")
#' 
#' # Run your model and get your predictions
#' no3_lm <- loadLm(
#'   formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
#'   data=fitdat, metadata=meta, retrans=exp)
#' 
#' # Now you can plot
#' plotEGRET("boxConcMonth", data = lamprey_nitrate, meta = meta)
#' plotEGRET("multiPlotDataOverview", load.model=no3_lm, newdata=estdat)
#' 
plotEGRET <- function(plot.name, 
                      load.model = NULL, newdata = NULL, data = NULL, meta = NULL, 
                      moreTitle = "loadflex", plotFlowNorm = FALSE, ...) {
  
  req_missing <- switch(
    plot.name,
    
    # require data & meta
    boxConcMonth = ,
    plotConcTime = ,
    plotConcQ = ,
    plotFluxQ =  
      is.null(load.model) && (is.null(data) || is.null(meta)),
    
    # require data, newdata, & meta
    boxQTwice = ,
    multiPlotDataOverview = 
      (is.null(load.model) && (is.null(data) || is.null(meta))) || is.null(newdata),
    
    # require load.model and newdata
    plotConcTimeDaily = ,
    plotFluxTimeDaily = ,
    plotConcPred = , 
    plotFluxPred = , 
    plotResidPred = ,
    plotResidQ = ,  
    plotResidTime = ,
    boxResidMonth = ,
    boxConcThree = , 
    plotConcHist = , 
    plotFluxHist = ,
    fluxBiasMulti = 
      is.null(load.model) || is.null(newdata),
    
    # default if no name matches
    FALSE)
  
  if(req_missing) {
    stop(paste0("missing data requirements for ", plot.name, ". See ?plotEGRET"))
  }
  
  egretobj <- convertToEGRET(load.model, newdata, data, meta)
  
  switch(
    plot.name,
    
    # require data & meta
    boxConcMonth = boxConcMonth(egretobj, ...),
    plotConcTime = plotConcTime(egretobj, ...),
    plotConcQ = plotConcQ(egretobj, ...),
    plotFluxQ = plotFluxQ(egretobj, ...),
    
    # require data, meta, newdata, and preds
    boxQTwice = boxQTwice(egretobj, ...),
    multiPlotDataOverview = multiPlotDataOverview(egretobj, ...),
    plotConcTimeDaily = plotConcTimeDaily(egretobj, ...),
    plotFluxTimeDaily = plotFluxTimeDaily(egretobj, ...),
    plotConcPred = plotConcPred(egretobj, ...), 
    plotFluxPred = plotFluxPred(egretobj, ...), 
    plotResidPred = plotResidPred(egretobj, ...),
    plotResidQ = plotResidQ(egretobj, ...),  
    plotResidTime = plotResidTime(egretobj, ...),
    boxResidMonth = boxResidMonth(egretobj, ...),
    boxConcThree = boxConcThree(egretobj, ...), 
    plotConcHist = plotConcHist(egretobj, plotFlowNorm = plotFlowNorm, ...), 
    plotFluxHist = plotFluxHist(egretobj, plotFlowNorm = plotFlowNorm, ...),
    fluxBiasMulti = fluxBiasMulti(egretobj, moreTitle = moreTitle, ...),
    
    # default if no name matches
    stop(paste('unrecognized plot.name:', plot.name)))
  
}
