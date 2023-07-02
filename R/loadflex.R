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
#' @name loadflex
#' @docType package
NULL

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(c(
    strwrap(paste(
      "loadflex is a USGS Archive Research Package:",
      "https://owi.usgs.gov/R/packages.html#research")),
    '',
    strwrap(paste(
      "Project funding has ended and our maintenance time is limited,",
      "but we do attempt to provide bug fixes and lightweight support as we are able.",
      "Submit questions or suggestions to https://github.com/USGS-R/loadflex/issues.")),
    '',
    c("In summer or fall 2023, this package will move from",
      "https://github.com/USGS-R/loadflex to",
      "https://github.com/DOI-USGS/loadflex",
      "Please update your links accordingly.")),
    collapse='\n'))
}

