#' getResiduals return the residuals of the load.model
#' 
#' @param load.model a loadModel descendant
#' @param flux.or.conc The format in which residuals should be calculated
#' @param abs.or.rel.resids Should residuals be computed as the difference 
#'   ("absolute") or the ratio ("relative") of the observed and predicted 
#'   values?
#' @param use.log logical. Should residuals be computed in log space (TRUE) or 
#'   linear space (FALSE)?
#' @param newdata New data for prediction and calculation of residuals
#' @param raw logical. [not yet implemented] If TRUE, the residuals are 
#'   calculated for predictions as they emerge from the inner model. If FALSE, 
#'   those outputs are retransformed if appropriate (e.g., from log to linear
#'   space) and converted to the specified format (flux or conc).
#' @return return The residuals between model predictions and the observations 
#'   in newdata
#' @export
getResiduals <- function(load.model, flux.or.conc=c("flux","conc"), 
                         abs.or.rel.resids=c("absolute","relative"), use.log=FALSE, 
                         newdata=NULL, raw=FALSE) {
  
  # Validate arguments
  flux.or.conc <- match.arg.loadflex(flux.or.conc)
  abs.or.rel.resids <- match.arg.loadflex(abs.or.rel.resids)
  
  # Get newdata from the model if not provided as an argument
  if(is.null(newdata)) newdata <- getFittingData(load.model)
  
  # Make sure newdata is ordered by date
  newdata <- newdata[order(getCol(getMetadata(load.model), newdata, "date")),]
  
  # Compute residuals as requested
  preds <- predictSolute(load.model=load.model, flux.or.conc=flux.or.conc, newdata=newdata, attach.units=FALSE)
  obs <- observeSolute(newdata, flux.or.conc, getMetadata(load.model), attach.units=FALSE)
  if(use.log) {
    preds <- log(preds)
    obs <- log(obs)
  }
  resids <- switch(
    abs.or.rel.resids,
    "absolute" = obs - preds,
    "relative" = obs / preds)
  
  # Return residuals packaged with their dates
  data.frame(Date=getCol(getMetadata(load.model), newdata, "date"), Resid=resids)
}