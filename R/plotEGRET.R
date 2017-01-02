#' Create an EGRET-style plot
#'  
#' Use a loadflex object, but create an EGRET plot.
#' 
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: staAbbr)
#' @param plotName the name of the plot the user wants to create. Current
#' options: ConcTime, ConcQ, FluxTimeDaily, and FluxQ.
#' @param ... additional arguments to pass to the plot
#'
#' @export
plotEGRET <- function(intdat, estdat, preds, meta, plotName, ...){
  
  egretobj <- convertToEGRET(intdat, estdat, preds, meta)
  
  switch(plotName,
         ConcTime = plotConcTime(egretobj, ...),
         ConcQ = plotConcQ(egretobj, ...),
         FluxTimeDaily = plotFluxTimeDaily(egretobj, ...),
         FluxQ = plotFluxQ(egretobj, ...))
  
}