# Make flux or concentration predictions from a loadModel model.

Makes instantaneous predictions (at the temporal resolution of
`newdata`) from a fitted
[`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md)
model. See
[`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)
for details.

## Usage

``` r
# S3 method for class 'loadModel'
predictSolute(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata = getFittingData(load.model),
  date = TRUE,
  count = !identical(agg.by, "unit"),
  se.fit = FALSE,
  se.pred = FALSE,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  lin.or.log = c("linear", "log"),
  agg.by = c("unit", "day", "month", "water year", "calendar year", "total", "[custom]"),
  na.rm = FALSE,
  attach.units = FALSE,
  ...
)
```

## Arguments

- load.model:

  A loadModel object.

- flux.or.conc:

  character. Should the predictions be reported as flux rates or
  concentrations? If the output is a data.frame, the column name for
  flux predictions will be "fit" when \`agg.by='unit'\` and "Flux_Rate"
  otherwise; the column name for concentration predictions will be "fit"
  when \`agg.by='unit'\` and "Conc" otherwise.

- newdata:

  `data.frame`, optional. Predictor data. Column names should match
  those given in the `loadModel` metadata. If `newdata` is not supplied,
  the original fitting data will be used.

- date:

  logical. If TRUE, the output data.frame will include a column
  containing the dates or grouping variables of the predictions. For
  agg.by=="unit", the resulting column will be "date"; for
  agg.by=="water year", the column will be "water.year", and so on.

- count:

  logical. If TRUE, and if agg.by!='unit', the output data.frame will
  include a column named 'count' containing the number of unit
  predictions going into each aggregated prediction (row).

- se.fit:

  logical. If TRUE, the output data.frame will include a column named
  "se.fit" describing the standard error of the model fit for each row
  of predictors.

- se.pred:

  logical. If TRUE, the output data.frame will include a column named
  "se.pred" describing the standard error of the prediction for each row
  of predictors. The se.pred values are standard errors of prediction
  (SEPs) and take into account not only the parameter uncertainty
  associated with the model coefficients (also covered by se.fit), but
  also the random error associated with any given observation (the
  epsilon term in a typical regression model).

- interval:

  character. One of "none", "confidence", or "prediction". If not
  "none", the interval bounds will be returned in columns named
  "lwr.fit" and "upr.fit" (for confidence intervals) or "lwr.pred" and
  "upr.pred" (for prediction intervals). Confidence intervals describe
  confidence in the model prediction for the mean value given a set of
  predictors, whereas prediction bounds describe the expected
  distribution of observations at that prediction point.

- level:

  numeric. Fraction of density distribution to include within the
  confidence or prediction interval

- lin.or.log:

  character. Either "linear" or "log" to say whether the predictions
  should be converted to log space or not. If converted to log space, a
  bias correction will be applied to regression model predictions; see
  [`linToLog`](http://doi-usgs.github.io/loadflex/reference/lognormal-moments.md).

- agg.by:

  character. The date interval or other grouping variable to aggregate
  results by. To do no aggregation, use the default of
  \`agg.by='unit'\`. If agg.by is one of "day", "month", "water year",
  or "calendar year", the dates vector will be split into periods
  corresponding to those intervals, and the flux or concentration will
  be computed for each period. If agg.by="total", `dates` will be
  ignored and the entire vector `preds` will be aggregated, weighting
  each point prediction equally. If agg.by is set to one or more custom
  values not in the list of default values, the values should be names
  of columns in \`newdata\`, and predictions will be grouped according
  to the values in those columns.

- na.rm:

  logical. Should NA values be removed before aggregation (TRUE), or
  should NA be returned for intervals that contain one or more NA
  predictions (FALSE)?

- attach.units:

  logical. Should the units be attached to columns in the resulting
  data.frame?

- ...:

  Additional arguments passed to class-specific implementations of the
  `predictSolute` generic function.

## Value

A vector of data.frame of predictions, as for the generic
[`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md).

## See also

Other predictSolute:
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`predictSolute.loadComp()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadComp.md),
[`predictSolute.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadInterp.md),
[`predictSolute.loadLm()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadLm.md),
[`predictSolute.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadReg2.md)
