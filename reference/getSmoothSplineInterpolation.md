# Generate a smoothing spline function with the parameters of your choice.

Produces an interpolation function of the form described in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md).

## Usage

``` r
getSmoothSplineInterpolation(...)
```

## Arguments

- ...:

  any arguments other than `x` and `y` to be passed to
  `stats::`[`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html).

## Value

A function of the form described in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md),
i.e., accepting the arguments `dates.in`, `y.in`, and `dates.out` and
returning predictions from a smooth spline function for `y.out`. That
function will use the arguments supplied in `...`.
