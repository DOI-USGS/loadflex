#' Models and Tools for Watershed Flux Estimates
#' 
#' @section Methods:
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
#' @section Installation:
#'   
#'   loadflex makes use of packages that are currently only available from 
#'   GitHub. To install these packages, run the following lines:
#'   
#'   library(devtools)
#'   
#'   install.packages(c("USGSwsData", "USGSwsDataRetrieval", "USGSwsBase", 
#'   "USGSwsGraphs", "USGSwsStats", "USGSwsQW"), 
#'   repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), 
#'   dependencies=TRUE, type="both")
#'   
#'   install_github("USGS-R/rloadest")
#'   
#'   install_github("appling/unitted")
#'   
#'   install_github("mcdowelllab/loadflex")
#'   
#' @name loadflex
#' @docType package
NULL