#' Interpolation functions
#' 
#' A set of functions that interpolate a y variable over time. See the 
#' Interpolation Types section for details specific to each function. These 
#' functions are intended for use within \code{\link{loadInterp}} and
#' \code{\link{loadComp}} models.
#' 
#' @details
#' 
#' These functions may assume the following without checking:
#' 
#' \itemize{
#' 
#' \item dates.in is sorted chronologically
#' 
#' \item y.in is sorted by dates.in
#' 
#' \item dates.in and dates.out are in the same numeric format
#' 
#' }
#' 
#' \subsection{Interpolation Methods}{}
#' 
#' \code{linearInterpolation} - straight lines from one point in y.in to the 
#' next.
#' 
#' @rdname interpolations
#' @name interpolations
#' @seealso \code{\link{getSmoothSplineInterpolation}} and 
#'   \code{\link{getDistanceWeightedInterpolation}} produce interpolation 
#'   functions of the form described here, but with flexibility in the values of
#'   arguments passed to the internal workhorse functions.
#' @references Verma, S., M. Markus, and R. A. Cooke. 2012. Development of error
#'   correction techniques for nitrate-N load estimation methods. Journal of 
#'   Hydrology 432-433:12-25.
#'   
#' @param dates.in A numeric vector desribing the dates for each of the values 
#'   in \code{y.in}. Dates are represented as the number of seconds since 1970.
#' @param y.in A vector of values (typically fluxes or concentrations) to 
#'   interpolate among.
#' @param dates.out A numeric vector of dates for which the corresponding output
#'   values are to be produced. Dates are represented as the number of seconds 
#'   since 1970.
#' @return A vector of values (typically fluxes or concentrations), in the same 
#'   units and on the same scale as the \code{y.in} values, that are the 
#'   function's predictions for \code{y.out} at each of the dates in dates.out.
#' @export
linearInterpolation <- function(dates.in, y.in, dates.out) {
  approx(x=dates.in, y=y.in, xout=dates.out, method="linear", rule=2)$y 
}


#' @details
#' 
#' \code{triangularInterpolation} - connects each successive pair of points 
#' \code{i} and \code{j} by a straight line from \code{(dates.in[i], y.in[i])} 
#' to \code{(mean(dates.in[i], dates.in[j]), 0)} to \code{(dates.in[j], 
#' y.in[j])}. This function was described by Verma et al. 2012 as an option for
#' residuals interpolation with the composite method.
#' 
#' @rdname interpolations
#' @inheritParams interpolations
#' @export
triangularInterpolation <- function(dates.in, y.in, dates.out) {
  genericTriangularInterpolation(dates.in, y.in, dates.out, y.mid=mean(y.in))
}

#' Generate a triangular interpolation function with the parameters of your 
#' choice.
#' 
#' Produces an interpolation function of the form described in 
#' \link{interpolations}.
#' 
#' @param y.mid The value or values to which midpoints between observations 
#'   should be set. Good choices are: mean(y.in) for stand-alone interpolations;
#'   0 for composite method interpolations among absolute residuals (in log or
#'   linear space); 1 for composite method interpolations among relative
#'   residuals (again, log or linear space).
#' @return A function of the form described in \link{interpolations}, i.e., 
#'   accepting the arguments \code{dates.in}, \code{y.in}, and \code{dates.out} 
#'   and returning predictions from a triangular interpolation function for 
#'   \code{y.out}. That function will use the \code{y.mid} argument supplied 
#'   here.
#' @export
getTriangularInterpolation <- function(y.mid=0) {
  function(dates.in, y.in, dates.out) {
    genericTriangularInterpolation(dates.in, y.in, dates.out, y.mid)
  }
}

#' A parameterizable triangular interpolation function.
#' 
#' Does not strictly adhere to the guidelines in \link{interpolations}, but can 
#' be used by \code{\link{geTriangularInterpolation}} to produce a 
#' function that does.
#' 
#' @inheritParams interpolations
#' @inheritParams getTriangularInterpolation
genericTriangularInterpolation <- function(dates.in, y.in, dates.out, y.mid) {
  # Determine x values: interleave dates.in and their midpoints and remove the
  # last element (a false midpoint, stored temporarily as NA)
  num_midpoints <- length(dates.in)-1
  midpoints <- dates.in + diff(dates.in)[c(1:num_midpoints, NA)]/2
  dates.in.interp <- c(rbind(dates.in, midpoints))
  false_midpoint <- which(is.na(dates.in.interp))
  dates.in.interp <- dates.in.interp[-false_midpoint]
  
  # Determine y values: interleave y.mids among the y.in values
  y.in.interp <- c(rbind(y.in, y.mid))[-false_midpoint]
  
  # Run a linear interpolation among the sequence of points and middles
  approx(x=dates.in.interp, y=y.in.interp, xout=dates.out, method="linear", rule=2)$y 
}

#' @details
#' 
#' \code{rectangularInterpolation} - horizontal lines through each point 
#' connected by vertical lines at the midpoint between each pair of dates. This
#' function was described and recommended by Verma et al. 2012 as an option for
#' residuals interpolation with the composite method.
#' 
#' @rdname interpolations
#' @inheritParams interpolations
#' @export
rectangularInterpolation <- function(dates.in, y.in, dates.out) {
  
  # At each midpoint between a pair of observations, create 2
  # nearly-simultaneous points having the y values of their left and right
  # neighboring observations, respectively.

  # Determine x values: interleave dates.in and their near-duplicated midpoints 
  # and remove the last elements (a false pair of midpoints, stored temporarily 
  # as NAs). For separating each pair of midpoints, we need a number
  # (tiny_number) that's small enough to make all predictions correctly, but not
  # too small to disappear in number storage
  midpoints1 <- dates.in + c(diff(dates.in), NA)/2
  tiny_number <- if(length(dates.out) > 1) min(diff(dates.out))/2 else 0.5 # actually, 0.5 should pretty much always be sufficient since dates are in as.numeric(POSIXct), so units = seconds
  midpoints2 <- midpoints1 + tiny_number
  dates.in.interp <- c(rbind(dates.in, midpoints1, midpoints2))
  dates.in.interp <- dates.in.interp[-which(is.na(dates.in.interp))]
  
  # Determine y values: replicate the y.in values to make horizontal lines 
  # between midpoints, removing the first and last elements because these are 
  # unnecessary midpoint values
  y.in.interp <- rep(y.in, each=3)[-c(1, 3*length(y.in))]
  
  # Run a linear interpolation among the sequence of points and middles
  approx(x=dates.in.interp, y=y.in.interp, xout=dates.out, method="linear", rule=2)$y
  
}


#' @details
#' 
#' \code{splineInterpolation} - a smooth curve that runs through each point in 
#' y.in.
#' 
#' @importFrom splines interpSpline
#' @rdname interpolations
#' @inheritParams interpolations
#' @export
splineInterpolation <- function(dates.in, y.in, dates.out) {
  predict(splines::interpSpline(dates.in, y.in), dates.out)$y
}


#' @details
#' 
#' \code{smoothSplineInterpolation} - a smoothing spline that follows the trends
#' in y.in without passing through each point. This function always uses a fixed
#' set of arguments for smooth.spline (mostly the defaults), while 
#' \code{\link{getSmoothSplineInterpolation}} can produce a smooth spline 
#' function with the parameters of your choice.
#' 
#' @rdname interpolations
#' @inheritParams interpolations
#' @export
smoothSplineInterpolation <- function(dates.in, y.in, dates.out) {
  genericSmoothSplineInterpolation(dates.in, y.in, dates.out, keep.data=FALSE)
}

#' Generate a smoothing spline function with the parameters of your choice.
#' 
#' Produces an interpolation function of the form described in \link{interpolations}.
#' 
#' @param ... any arguments other than \code{x} and \code{y} to be passed to 
#'   \code{stats::\link{smooth.spline}}.
#' @return A function of the form described in \link{interpolations}, i.e., 
#'   accepting the arguments \code{dates.in}, \code{y.in}, and \code{dates.out} 
#'   and returning predictions from a smooth spline function for \code{y.out}. 
#'   That function will use the arguments supplied in \code{...}.
#' @export
getSmoothSplineInterpolation <- function(...) {
  function(dates.in, y.in, dates.out) {
    genericSmoothSplineInterpolation(dates.in, y.in, dates.out, ...)
  }
}

#' A parameterizable smoothing spline function.
#' 
#' Does not strictly adhere to the guidelines in \link{interpolations}, but can 
#' be used by \code{\link{getSmoothSplineInterpolation}} to produce a 
#' function that does.
#' 
#' @importFrom stats smooth.spline
#' @inheritParams interpolations
#' @inheritParams getSmoothSplineInterpolation
genericSmoothSplineInterpolation <- function(dates.in, y.in, dates.out, ...) {
  predict(smooth.spline(dates.in, y.in, ...), dates.out)$y
}

#' @details
#' 
#' \code{distanceWeightedInterpolation} - An inverse-distance-weighted average 
#' of y.in at each dates.out point. This function creates a weight for each y.in
#' based on the distance from dates.in to the values in dates.out. The y.out 
#' prediction for each value of dates.out is then the weighted average of the 
#' y.in values. This function always uses a fixed inverse-distance function (1/(a-b)^2),
#' while \code{\link{getDistanceWeightedInterpolation}} can apply the
#' inverse-distance function of your choice.
#' 
#' @rdname interpolations
#' @inheritParams interpolations
#' @export
distanceWeightedInterpolation <- function(dates.in, y.in, dates.out) {
  genericDistanceWeightedInterpolation(dates.in, y.in, dates.out)
}

#' Generate a distance-weighted interpolation function with the parameters of your choice.
#' 
#' Produces an interpolation function of the form described in \link{interpolations}.
#' 
#' @param inv.dist.fun A function to calculate an inverse distance metric.
#'   Should be vectorized such that one of \code{a} or \code{b} may be a vector
#'   when the other is a scalar.
#' @export
getDistanceWeightedInterpolation <- function(inv.dist.fun=function(a,b) { 1 / ((a-b)^2) }) {
  function(dates.in, y.in, dates.out) {
    genericDistanceWeightedInterpolation(dates.in, y.in, dates.out, inv.dist.fun)
  }
}

#' A parameterizable distance-weighted interpolation function.
#' 
#' Does not strictly adhere to the guidelines in \link{interpolations}, but can 
#' be used by \code{\link{getDistanceWeightedInterpolation}} to produce a 
#' function that does.
#'   
#' @inheritParams interpolations
#' @inheritParams genericDistanceWeightedInterpolation
genericDistanceWeightedInterpolation <- function(dates.in, y.in, dates.out, inv.dist.fun=function(a,b) { 1 / ((a-b)^2) }) {

  # Iterate over the dates.out to avoid creating a giant matrix of size
  # length(dates.in) * length(dates.out)
  sapply(dates.out, function(x) {
    # Calculate the inverse distance using the specified function. A common
    # inverse distance function is function(a,b) { 1 / ((a-b)^2) }.
    inv_dist <- inv.dist.fun(x, dates.in)

    # Normalize weights so they sum to 1 (except in case of infinite values,
    # addressed next)
    weights <- inv_dist/sum(inv_dist)
    
    # If there are infinite inverse distances (distance == 0), then all but 
    # those cells will have been set to 0 by the normalization. Identify those
    # cells and partition their weights by the number of such cells (usually
    # just one of them) so that sum(weights) equals 1 when we're done.
    infinite_distances <- which(is.infinite(inv_dist))
    weights <- replace(weights, infinite_distances, 1/length(infinite_distances))

    # Now compute and return the weighted sum of the y.in values
    sum(weights * y.in) 
  })
  
}
