# get rho matrix first order

get rho matrix first order

## Usage

``` r
getCormatFirstOrder(
  rho,
  time.step = as.difftime(1, units = "hours"),
  max.tao = as.difftime(1, units = "days")
)
```

## Arguments

- rho:

  the covariance asdefined as difference between the times divided by
  the time step

- time.step:

  default is 1 hour

- max.tao:

  don't consider covariance for values further apart then this.

## Value

covariance defined as difference between the times divided by the time
step don't calculate covariance for values further away then max.tao.
