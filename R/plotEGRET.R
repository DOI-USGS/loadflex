#' Create an EGRET-style plot
#'  
#' Use a loadflex object, but create an EGRET plot.
#' 
#' @param plot.name the name of the plot the user wants to create. Current
#' options: ConcTime, ConcQ, FluxTimeDaily, and FluxQ. Each require different
#' data, see Details.
#' @param intdat data.frame of interpolation data
#' @param estdat data.frame of estimation data
#' @param preds data.frame of load predictions
#' @param meta loadflex metadata object; it must include constituent,
#' flow, conc.units, custom (station abbreviation: sta.abbr, and a short
#' name for the constituent: consti.name)
#' @param ... additional arguments to pass to the plot
#'
#' @details ConcTime, ConcQ, and FluxQ require \code{intdat, meta}. FluxTimeDaily
#' requires \code{intdat, estdat, preds, meta}.
#'
#' @importFrom EGRET plotConcTime
#' @importFrom EGRET plotConcQ
#' @importFrom EGRET plotFluxTimeDaily
#' @importFrom EGRET plotFluxQ
#'
#' @export
plotEGRET <- function(plot.name, intdat = NULL, estdat = NULL, preds = NULL, meta = NULL, ...) {
  
  req_missing <- switch(plot.name,
                        ConcTime = missing(intdat) | missing(meta),
                        ConcQ = missing(intdat) | missing(meta),
                        FluxTimeDaily = missing(intdat) | missing(meta) |
                          missing(estdat) | missing(preds),
                        FluxQ = missing(intdat) | missing(meta),
                        FALSE)
  
  if(req_missing){
    stop(paste0("missing data requirements for ", plot.name, ". See ?plotEGRET"))
  }
  
  egretobj <- convertToEGRET(intdat, estdat, preds, meta)
  
  switch(plot.name,
         ConcTime = plotConcTime(egretobj, ...),
         ConcQ = plotConcQ(egretobj, ...),
         FluxTimeDaily = plotFluxTimeDaily(egretobj, ...),
         FluxQ = plotFluxQ(egretobj, ...),
         stop(paste('unrecognized plot.name:', plot.name)))
  
}
