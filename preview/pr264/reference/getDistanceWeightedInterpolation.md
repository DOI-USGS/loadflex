# Generate a distance-weighted interpolation function with the parameters of your choice.

Produces an interpolation function of the form described in
[interpolations](http://doi-usgs.github.io/loadflex/reference/interpolations.md).

## Usage

``` r
getDistanceWeightedInterpolation(
  inv.dist.fun = function(a, b) {
1/((a - b)^2)
 }
)
```

## Arguments

- inv.dist.fun:

  A function to calculate an inverse distance metric. Should be
  vectorized such that one of `a` or `b` may be a vector when the other
  is a scalar.
