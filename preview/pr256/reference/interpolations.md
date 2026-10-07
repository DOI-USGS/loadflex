# Interpolation functions

A set of functions that interpolate a y variable over time. See the
Interpolation Types section for details specific to each function. These
functions are intended for use within
[`loadInterp`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md)
and
[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
models.

## Usage

``` r
linearInterpolation(dates.in, y.in, dates.out)

triangularInterpolation(dates.in, y.in, dates.out)

rectangularInterpolation(dates.in, y.in, dates.out)

splineInterpolation(dates.in, y.in, dates.out)

smoothSplineInterpolation(dates.in, y.in, dates.out)

distanceWeightedInterpolation(dates.in, y.in, dates.out)
```

## Arguments

- dates.in:

  A numeric vector desribing the dates for each of the values in `y.in`.
  Dates are represented as the number of seconds since 1970.

- y.in:

  A vector of values (typically fluxes or concentrations) to interpolate
  among.

- dates.out:

  A numeric vector of dates for which the corresponding output values
  are to be produced. Dates are represented as the number of seconds
  since 1970.

## Value

A vector of values (typically fluxes or concentrations), in the same
units and on the same scale as the `y.in` values, that are the
function's predictions for `y.out` at each of the dates in dates.out.

## Details

These functions may assume the following without checking:

- dates.in is sorted chronologically

- y.in is sorted by dates.in

- dates.in and dates.out are in the same numeric format

`linearInterpolation` - straight lines from one point in y.in to the
next.

`triangularInterpolation` - connects each successive pair of points `i`
and `j` by a straight line from `(dates.in[i], y.in[i])` to
`(mean(dates.in[i], dates.in[j]), 0)` to `(dates.in[j], y.in[j])`. This
function was described by Verma et al. 2012 as an option for residuals
interpolation with the composite method.

`rectangularInterpolation` - horizontal lines through each point
connected by vertical lines at the midpoint between each pair of dates.
This function was described and recommended by Verma et al. 2012 as an
option for residuals interpolation with the composite method.

`splineInterpolation` - a smooth curve that runs through each point in
y.in.

`smoothSplineInterpolation` - a smoothing spline that follows the trends
in y.in without passing through each point. This function always uses a
fixed set of arguments for smooth.spline (mostly the defaults), while
[`getSmoothSplineInterpolation`](http://doi-usgs.github.io/loadflex/reference/getSmoothSplineInterpolation.md)
can produce a smooth spline function with the parameters of your choice.

`distanceWeightedInterpolation` - An inverse-distance-weighted average
of y.in at each dates.out point. This function creates a weight for each
y.in based on the distance from dates.in to the values in dates.out. The
y.out prediction for each value of dates.out is then the weighted
average of the y.in values. This function always uses a fixed
inverse-distance function (1/(a-b)^2), while
[`getDistanceWeightedInterpolation`](http://doi-usgs.github.io/loadflex/reference/getDistanceWeightedInterpolation.md)
can apply the inverse-distance function of your choice.

## References

Verma, S., M. Markus, and R. A. Cooke. 2012. Development of error
correction techniques for nitrate-N load estimation methods. Journal of
Hydrology 432-433:12-25.

## See also

[`getSmoothSplineInterpolation`](http://doi-usgs.github.io/loadflex/reference/getSmoothSplineInterpolation.md)
and
[`getDistanceWeightedInterpolation`](http://doi-usgs.github.io/loadflex/reference/getDistanceWeightedInterpolation.md)
produce interpolation functions of the form described here, but with
flexibility in the values of arguments passed to the internal workhorse
functions.
