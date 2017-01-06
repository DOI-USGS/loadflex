#' Create an EGRET-style plot
#'  
#' @description Use loadflex data frames, but create an EGRET plot.
#' 
#' @param plot.name the name of the plot the user wants to create. See 
#' Details for current options. For now, only one allowed at time.
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
#' @param preds.type character specifying if the predictions being used are
#' concentrations ("Conc") or fluxes ("Flux").
#' @param ... additional arguments to pass to the plot
#'
#' @details EGRET plots that require \code{intdat, meta}:
#' \itemize{
#'   \item boxConcMonth
#'   \item plotConcTime
#'   \item plotConcQ
#'   \item plotFluxQ
#' }
#' EGRET plots that require \code{intdat, estdat, preds, meta}:
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
#' intdat <- lamprey_nitrate
#' 
#' data("lamprey_discharge")
#' estdat <- lamprey_discharge # estimation data (15 min interval)
#' estdat <- subset(estdat, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT")) 
#' estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # only keep 4 observations per day
#' 
#' meta <- metadata(constituent="NO3", 
#'                  flow="DISCHARGE",
#'                  dates="DATE", 
#'                  conc.units="mg L^-1", 
#'                  flow.units="cfs", 
#'                  load.units="kg",
#'                  load.rate.units="kg d^-1", 
#'                  station="Lamprey River, NH",
#'                  custom=list(sta.abbr = "lamp",
#'                              consti.name = "nitrate"))
#' 
#' # Run your model and get your predictions
#' no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
#'                  data=intdat, metadata=meta, retrans=exp)
#' preds <- predictSolute(no3_lm, "conc", estdat, se.pred=TRUE, date=TRUE)
#' 
#' # Now you can plot
#' plotEGRET("boxConcMonth", intdat = lamprey_nitrate, meta = meta)
#' plotEGRET("multiPlotDataOverview", lamprey_nitrate, estdat, preds, meta)
#' 
plotEGRET <- function(plot.name, intdat = NULL, estdat = NULL, preds = NULL, 
                      meta = NULL, preds.type = "Conc", ...) {
  
  req_missing <- switch(plot.name,
                        
                        # require intdat & meta
                        boxConcMonth = ,
                        plotConcTime = ,
                        plotConcQ = ,
                        plotFluxQ = missing(intdat) | missing(meta),
                        
                        # require intdat, meta, estdat, and preds
                        boxQTwice = ,
                        multiPlotDataOverview = ,
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
                        fluxBiasMulti = missing(intdat) | missing(meta) |
                          missing(estdat) | missing(preds),
                        
                        # default if no name matches
                        FALSE)
  
  if(req_missing) {
    stop(paste0("missing data requirements for ", plot.name, ". See ?plotEGRET"))
  }
  
  egretobj <- convertToEGRET(intdat, estdat, preds, meta, preds.type)
  
  switch(plot.name,
         
         # require intdat & meta
         boxConcMonth = boxConcMonth(egretobj, ...),
         plotConcTime = plotConcTime(egretobj, ...),
         plotConcQ = plotConcQ(egretobj, ...),
         plotFluxQ = plotFluxQ(egretobj, ...),
         
         # require intdat, meta, estdat, and preds
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
         plotConcHist = plotConcHist(egretobj, ...), 
         plotFluxHist = plotFluxHist(egretobj, ...),
         fluxBiasMulti = fluxBiasMulti(egretobj, ...),
         
         # default if no name matches
         stop(paste('unrecognized plot.name:', plot.name)))
  
}
