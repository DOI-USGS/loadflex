# Estimate uncertainty in an interpolation using leave-one-out cross validation.

Calculates and returns the mean squared errors (MSEs) in the units and
transformation space (e.g., log space) of the left-hand side of the
model fit equation.

## Usage

``` r
# S3 method for class 'loadInterp'
estimateMSE(
  load.model,
  n.out,
  n.iter = floor(nrow(getFittingData(load.model))/n.out),
  replace,
  ...
)
```

## Arguments

- load.model:

  A load model object, typically inheriting from loadModel and always
  implementing the loadModelInterface.

- n.out:

  numeric. The number of observations in the fitting data to leave out
  in each iteration.

- n.iter:

  numeric. The number of iterations to perform.

- replace:

  logical. In each iteration, should the n.out observations that are
  left out be sampled with replacement of any previous sets of n.out
  observations (TRUE) or without replacement (FALSE)?

- ...:

  Other arguments passed to inheriting methods for estimateMSE

## Details

This method is leave-one-out cross validation (LOOCV) when n.out==1,
n.iter==nrow(getFittingData(load.model)), and with.replacement=FALSE.
This method is k-fold cross validation when
n.out\*n.iter==nrow(getFittingData(load.model)) and
with.replacement==FALSE.

## See also

Other estimateMSE:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`estimateMSE.loadComp()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadComp.md)
