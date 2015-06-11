# This file contains a list of functions, all with names starting with getPred_,
# that each produce a column of a derived predictor variable.

#' Create a vector of baseflow rates.
#' 
#' Internal for now; not yet ready to be a part of the official API
#' 
#' Given a dataset with date and flow columns, separates total flow into 
#' baseflow and runoff; returns a vector of baseflow rates. Several methods are 
#' available and can be selected with the "method" argument. All methods assume 
#' that the time series is evenly spaced, and all work better with 
#' higher-resolution data (e.g., hourly or daily rather than weekly or monthly).
#' 
#' method=="1p digital filter": Uses the one-paramter digital filter described 
#' by Nathan, R.J. and T.A. McMahon, 1990. Evaluation of Automated Techniques 
#' for Baseflow and Recession Analysis. Water Resources Research, 
#' 26(7):1465-1473. This method requires specification of the \code{alpha} 
#' parameter.
#' 
#' method=="2p digital filter": Uses the two-parameter digital filter described 
#' by Eckhardt, K., 2005. How to Construct Recursive Digital Filters for 
#' Baseflow Separation. Hydrological Processes, 19(2):507-515. This method 
#' requires specification of the \code{alpha} and \code{BFImax} parameters.
#' 
#' @param data data.frame with columns as specified by metadata
#' @param metadata object of class metadata from which Flow and Date columns 
#'   will be selected.
#' @param method character naming the method to be used. May be specified by the
#'   first character or characters, e.g., "h", "1", or "2". See details for a 
#'   description of each algorithm.
#' @param da numeric, required for method="hysep". As described in 
#'   ?DVstats::hysep, 'the drainage area of the basin in square miles.'
#' @param select character, optional argument to method="hysep". As described in
#'   ?DVstats::hysep, 'a character string indicating which method to use for the
#'   baseflow in the output dataset. Must be one of "sliding," "local minimum," 
#'   or "fixed." Onle the first letter is required.' See the HYSEP documentation
#'   by Sloto and Crouse at 
#'   http://water.usgs.gov/software/HYSEP/code/doc/hysep.pdf for method 
#'   descriptions.
#' @param alpha numeric, required argument for \code{method \%in\% c("1p digital
#'   filter","2p digital filter")}. Adjusts the strength of the filter, with 
#'   numbers closer to 1 leading to more of total flow being classified as 
#'   runoff (non-baseflow). For the 2-parameter digital filter, the default is 
#'   0.98.
#' @param BFImax numeric, required argumend for \code{method=="2p digital 
#'   filter"}. The maximum long-term fraction of total streamflow that may be 
#'   classified as baseflow. Recommended values are 0.8 for perennial streams 
#'   with porous aquifers, 0.50 for ephemeral streams with porous aquifers, and 
#'   0.25 for perennial streams with hard rock aquifers. (Defaults from 
#'   http://user.engineering.uiowa.edu/~flood/handouts/ 
#'   HO-L17-Baseflow-Separation.pdf)
#' @param ... other arguments passed to the specified method (currently only 
#'   'hysep' takes other arguments; see ?DVstats::hysep)
#'   
#' @references Implementation of the 1- and 2-parameter digital filters was 
#'   aided by the online lecture notes of A. Allen Bradley, Jr. for his Fall 
#'   2013 Hydrology course (http://user.engineering.uiowa.edu/~flood)
#' @family predictors
#' @keywords internal
#' @importFrom lubridate is.POSIXt is.Date
getPred_baseflow <- function(data, metadata, method=c("hysep","1p digital filter","2p digital filter"), 
                             da, select, alpha, BFImax, ...) {
  #silly thing needed to pass R CMD check
  # from http://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  #hysep<- 'method.var'
  method <- match.arg(method)
  Flows <- getCol(metadata, data, "flow", FALSE)
  Dates <- getCol(metadata, data, "date", FALSE)
  
  switch(
    method,
#     "hysep"={
#       # I've made hysep unavailable, at least for now, because it requires an
#       # additional USGS-R dependency that may not be used much. Can we provide a
#       # helper function to help the user prepare/process data for/from
#       # DVstats::hysep() rather than wrapping that function here?
#       
#       # This method wraps the DVstats::hysep() approach for separating baseflow and stormflow.
#       # Advantage: already implemented. Disadvantage: requires that dates are in the
#       # Date format, effectively limiting the resolution to <=1 observation per day.
#       
#       # roxygen2 documentation: 
#       #   method=="hysep": Uses the HYSEP model, and specifically the hysep function 
#       #   from the USGS package DVstats. This package must be installed before the 
#       #   method can be used. To install from the USGS GitHub page, call 
#       #   \code{library(devtools); install_github("DVstats",user="USGS-R")}. When using
#       #   the hysep() method, the user will likely want to choose among methods 
#       #   available within hysep(), which are specified by setting the \code{select} 
#       #   argument; see the parameter description for \code{select} for details. This 
#       #   method requires specification of the \code{da} and \code{select} parameters. 
#       #   The \code{hysep} method further requires that dates can be specified as dates
#       #   without hours, minutes, or seconds, i.e., that the time step is a multiple of
#       #   1 day.  
#       #
#       #   importFrom DVstats hysep
#       
#       # install_github("USGS-R/DVstats")
#       # library(DVstats)
#       
#       if(is.Date(Dates)) {
#         DateDates <- Dates
#       } else if(is.POSIXt(Dates)) {
#         warning("hysep() requires dates in Date format; coercing to Date")
#         DateDates <- as.Date(Dates, tz=Sys.timezone() )
#       } else if(is.character(Dates)) {
#         DateDates <- as.Date()
#       }
#       
#       # ChoptankFlow$BaseQ <- with(ChoptankFlow, 
#       baseflow <- hysep(Flows, Dates, da=da, select=select, ...)$BaseQ
#     },
    "1p digital filter"={
      # Implements the digital filter described at
      # http://user.engineering.uiowa.edu/~flood/handouts/HO-L17-Baseflow-Separation.pdf
      # and in Nathan, R.J. and T.A. McMahon, 1990. Evaluation of Automated
      # Techniques for Baseflow and Recession Analysis. Water Resources
      # Research, 26(7):1465-1473.
      runoff <- baseflow <- rep(0, length(Flows))
      baseflow[1] <- Flows[1] # must be in a baseflow period
      for(i in 2:length(Flows)) {
        runoff[i] <- alpha * runoff[i-1] + ((1 + alpha)/2) * (Flows[i] - Flows[i-1])
        if(runoff[i] < 0) runoff[i] <- 0
        if(runoff[i] > Flows[i]) runoff[i] <- Flows[i]
        baseflow[i] <- Flows[i] - runoff[i]
      }
    },
    "2p digital filter"={
      # Implements the 2-parameter digital fitler described at
      # http://user.engineering.uiowa.edu/~flood/handouts/HO-L17-Baseflow-Separation.pdf
      # and in Eckhardt, K., 2005. How to Construct Recursive Digital Filters
      # for Baseflow Separation. Hydrological Processes, 19(2):507-515.
      runoff <- baseflow <- rep(0, length(Flows))
      baseflow[1] <- Flows[1] # must be in a baseflow period
      for(i in 2:length(Flows)) {
        baseflow[i] <- ((1-BFImax)*alpha*baseflow[i-1] + (1-alpha)*BFImax*Flows[i]) / (1-alpha*BFImax)
        if(baseflow[i] > Flows[i]) baseflow[i] <- Flows[i]
        runoff[i] <- Flows[i] - baseflow[i]
      }
    })
  
  return(baseflow)
}

#' Create a logical vector indicating whether flow at each time point is 
#' dominantly baseflow.
#' 
#' Internal for now; not yet ready to be a part of the official API
#' 
#' @param data a data.frame of data including a column for flow (with that 
#'   column name specified by metadata).
#' @param metadata a metadata object specifying, at minimum, the column name in 
#'   \code{data} where total flow is stored.
#' @param baseflow a vector of baseflow values of the same length as 
#'   \code{nrow(data)}.
#' @param threshold numeric. The minimum fraction of flow that must be baseflow 
#'   for a time point to be classified as baseflow (TRUE).
#'   
#' @family predictors
#' @keywords internal
getPred_isBaseflow <- function(data, metadata, baseflow, threshold=0.8) {
  totalflow <- getCol(metadata, data, "flow", FALSE)
  return(baseflow/totalflow >= threshold)
}


# getPred_antecedentFlow <- function(Flow, Dates) {
#   
# }
# 
# getPred_antecedentFlowStats <- function(Flow, Dates) {
#   
# }