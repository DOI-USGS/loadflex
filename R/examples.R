#### Documentation ####

#' @name eg_loadflex
#' @aliases lamprey_discharge lamprey_nitrate eg_fitdat eg_estdat eg_metadata
#'   eg_loadInterp eg_loadLm eg_loadReg2 eg_loadComp
#' @title Example datasets and objects for the \pkg{loadflex} package
#' @description These datasets and pre-created objects are provided for 
#'   exploring and testing the \pkg{loadflex} package.
#' @section Datasets:
#'   
#'   \describe{
#'   
#'   \item{\code{eg_fitdat}}{Example dataset for fitting (calibrating) a model. 
#'   These data are a lightweight subset of \code{lamprey_nitrate}, below.}
#'   
#'   \item{\code{eg_estdat}}{Example dataset for generating predictions from a 
#'   fitted model. These data are a lightweight subset of 
#'   \code{lamprey_discharge}, below.}
#'   
#'   \item{\code{lamprey_discharge}}{Discharge data for the Lamprey River from 
#'   10/1/1999 to 11/16/2014. Discharge in CFS measured every  15 minutes and 
#'   collected by the US Geological Survey, site 01073500, waterdata.usgs.gov. 
#'   The Lamprey River is an 81-km river flowing through southeastern New 
#'   Hampshire. Its 548-km2 watershed empties into the Great Bay estuary.}
#'   
#'   \item{\code{lamprey_nitrate}}{Nitrate data for the Lamprey River from 
#'   10/1/1999 to 11/16/2014. Nitrate is in mg/L and is collected weekly. The 
#'   Lamprey River is an 81-km river flowing through southeastern New Hampshire.
#'   Its 548-km2 watershed empties into the Great Bay estuary. Nitrate 
#'   concentrations have been monitored with weekly and event-based grab samples
#'   at Packers Falls on the Lamprey since 10 September 1999 and are measured by
#'   SmartChem discrete analyzer (Westco, Brookfield, CT).}
#'   
#'   }
#'   
#' @section Objects:
#'   
#'   \describe{
#'   
#'   \item{\code{eg_metadata}}{Example metadata object.}
#'   
#'   \item{\code{eg_loadInterp}}{Example interpolation model object.}
#'   
#'   \item{\code{eg_loadLm}}{Example linear regression model object.}
#'   
#'   \item{\code{eg_loadReg2}}{Example model object containing an inner 
#'   \pkg{rloadest} model.}
#'   
#'   \item{\code{eg_loadComp}}{Example composite method model object.}
#'   
#'   }
#'   
#' @docType data
NULL

#### Example creation functions ####

# See data-raw/createExamples.R for most example creation code

#' @rdname loadflex-deprecated
#' @details `exampleMetadata` has been replaced by `data(eg_metadata)`
#' @md
#' @importFrom utils data
#' @export
exampleMetadata <- function() {
  .Deprecated('eg_metadata', package='loadflex',
              'exampleMetadata() takes too long; use data(eg_metadata) instead')
  eg_metadata_name <- data("eg_metadata", envir=environment())
  return(get(eg_metadata_name))
}

