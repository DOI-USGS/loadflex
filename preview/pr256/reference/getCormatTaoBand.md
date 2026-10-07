# Same idea as getCormatCustom(rho1DayBand, dates) but runs faster.

getcormatTaoBand

## Usage

``` r
getCormatTaoBand(max.tao = as.difftime(1, units = "days"))
```

## Arguments

- max.tao:

  length of the covariance band, defaults to 1 day

## Value

matrix of the covariances

## Details

calculate the covariance 1 if dates are within the tao band, 0 if they
are not Same idea as cormatrix(rho1DayBand, dates) but runs in linear
instead of quadratic time - a big and much-needed improvement.
