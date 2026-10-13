# Resample the coefficients from a loadReg model.

Dives deep into loadReg objects to replace the coefficients for the
purpose of simulating new predictions.

## Usage

``` r
resampleCoefficients.loadReg(fit, flux.or.conc)
```

## Arguments

- fit:

  a loadReg object

- flux.or.conc:

  Should the resampling be done for the coefficients appropriate to flux
  or those for concentration?

## Value

A new loadReg object with resampled coefficients such that one of
predConc or predLoad (corresponding to the value of `flux.or.conc`) will
make predictions reflecting those new coefficients. No other properties
of the returned model are guaranteed.
