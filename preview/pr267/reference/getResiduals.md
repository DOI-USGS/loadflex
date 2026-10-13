# getResiduals return the residuals of the load.model

getResiduals return the residuals of the load.model

## Usage

``` r
getResiduals(
  load.model,
  flux.or.conc = c("flux", "conc"),
  abs.or.rel.resids = c("absolute", "relative"),
  use.log = FALSE,
  newdata = NULL,
  raw = FALSE
)
```

## Arguments

- load.model:

  a loadModel descendant

- flux.or.conc:

  The format in which residuals should be calculated

- abs.or.rel.resids:

  Should residuals be computed as the difference ("absolute") or the
  ratio ("relative") of the observed and predicted values?

- use.log:

  logical. Should residuals be computed in log space (TRUE) or linear
  space (FALSE)?

- newdata:

  New data for prediction and calculation of residuals

- raw:

  logical. \[not yet implemented\] If TRUE, the residuals are calculated
  for predictions as they emerge from the inner model. If FALSE, those
  outputs are retransformed if appropriate (e.g., from log to linear
  space) and converted to the specified format (flux or conc).

## Value

return The residuals between model predictions and the observations in
newdata
