# Resample the coefficients of a linear model (lm)

Returns a new linear model given their original covariance and
uncertainty

## Usage

``` r
resampleCoefficients.lm(fit)
```

## Arguments

- fit:

  an lm object whose coefficients should be resampled

## Value

A new lm object with resampled coefficients such that predict.lm() will
make predictions reflecting those new coefficients. No other properties
of the returned model are guaranteed.

## Details

(Although the name suggests otherwise, resampleCoefficients is not
currently an S3 generic. You should refer to this function by its
complete name.)

## References

http://www.clayford.net/statistics/simulation-to-represent-uncertainty-in-regression-coefficients/
