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
#'   \code{install.packages(c("smwrData", "smwrBase", "smwrGraphs", "smwrStats", 
#'   "smwrQW", "rloadest", "unitted"), repos=c("https://owi.usgs.gov/R", 
#'   "https://cran.rstudio.com"), dependencies=TRUE, type="both")}
#'   
#'   \code{install.packages(c("car", "dplyr", "ggplot2", "lubridate", "MASS", 
#'   "Matrix"), dependencies=TRUE, type="both")}
#'   
#'   You'll also need the `devtools` package; see 
#'   \url{https://www.rstudio.com/products/rpackages/devtools/} for special
#'   instructions, and also run this command:
#'   
#'   \code{install.packages("devtools")}
#'   
#'   and lastly run this call to actually install `loadflex`:
#'   
#'   \code{devtools::install_github("USGS-R/loadflex")}
#'   
#' @name loadflex
#' @docType package
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
  is subject to revision. It is being provided to meet
  the need for timely best science. The information
  has not received final approval by the U.S. Geological
  Survey (USGS) and is provided on the condition that
  neither the USGS nor the U.S. Government shall be held
  liable for any damages resulting from the authorized
  or unauthorized use of the information.
  
  Funding for loadflex expires summer 2018, 
  after which bugfixes & new features will be minimal")
}
