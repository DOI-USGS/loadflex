#' Create an EGRET-style plot
#'  
#' Use a loadflex object, but create an EGRET plot.
#' 
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
#' @param plot.name the name of the plot the user wants to create. Current
#' options: ConcTime, ConcQ, FluxTimeDaily, and FluxQ.
#' @param ... additional arguments to pass to the plot
#'
#' @importFrom EGRET plotConcTime
#' @importFrom EGRET plotConcQ
#' @importFrom EGRET plotFluxTimeDaily
#' @importFrom EGRET plotFluxQ
#'
#' @export
plotEGRET <- function(intdat, estdat, preds, meta, plot.name, ...) {
  
  egretobj <- convertToEGRET(intdat, estdat, preds, meta)
  
  switch(plot.name,
         ConcTime = plotConcTime(egretobj, ...),
         ConcQ = plotConcQ(egretobj, ...),
         FluxTimeDaily = plotFluxTimeDaily(egretobj, ...),
         FluxQ = plotFluxQ(egretobj, ...),
         stop(paste('unrecognized plot.name:', plot.name)))
  
}
