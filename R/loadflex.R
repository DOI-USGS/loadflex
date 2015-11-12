#' Models and Tools for Watershed Flux Estimates
#' 
#' @section Models:
#'   
#'   \itemize{
#'   
#'   \item Linear and period-weighted interpolations: \code{\link{loadInterp}}
#'   
#'   \item Generic regression-based models: \code{\link{loadModel}}
#'   
#'   \item Regression-based models with rloadest (USGS, from LOADEST): 
#'   \code{\link{loadReg2}}
#'   
#'   \item Composite-method-based models: \code{\link{loadComp}}
#'   
#'   }
#'   
#' @section Predictions:
#'   
#'   \itemize{
#'   
#'   \item Make predictions at the resolution of the predictor data: 
#'   \code{\link{predictSolute}}
#'   
#'   \item Collect predictions into means or totals over longer time periods: 
#'   \code{\link{aggregateSolute}}
#'   
#'   }
#'   
#' @section Installation:
#'   
#'   loadflex makes use of packages that are currently only available from 
#'   GitHub or the USGS R package repository. To install these packages, run the
#'   following lines:
#'   
#'   install.packages(c("smwrData", "smwrBase", "smwrGraphs", "smwrStats", 
#'   "smwrQW", "rloadest"), repos=c("http://owi.usgs.gov/R", 
#'   "http://cran.us.r-project.org"), dependencies=TRUE, type="both")
#'   
#'   devtools::install_github(c("appling/unitted", "mcdowelllab/loadflex"))
#'   
#' @name loadflex
#' @docType package
NULL