# Make flux or concentration predictions from a loadInterp model.

Makes instantaneous predictions (at the temporal resolution of
`newdata`) from a fitted
[`loadInterp`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md)
model. See
[`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)
for details. For loadInterp models in particular,

## Usage

``` r
# S3 method for class 'loadInterp'
predictSolute(
  load.model,
  flux.or.conc = c("flux", "conc"),
  newdata,
  date = TRUE,
  count = TRUE,
  se.fit = FALSE,
  se.pred = FALSE,
  interval = c("none", "prediction"),
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

  A loadInterp object.

- flux.or.conc:

  character. Should the predictions be reported as flux rates or
  concentrations? If the output is a data.frame, the column name for
  flux predictions will be "fit" when \`agg.by='unit'\` and "Flux_Rate"
  otherwise; the column name for concentration predictions will be "fit"
  when \`agg.by='unit'\` and "Conc" otherwise.

- newdata:

  `data.frame`, optional. Predictor data. Because loadInterp models
  interpolate in time among the observations to which they have been
  "fitted", `newdata` is usually simply a one-column data.frame of dates
  or date-times. Column names should match those given in the
  `loadInterp` metadata. If `newdata` is not supplied, the original
  fitting data will be used.

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

  logical. Must be FALSE because se.fit is unavailable for loadInterp
  models.

- se.pred:

  logical. If TRUE, the output data.frame will include a column named
  "se.pred" describing the standard error of the prediction for each row
  of predictors. The se.pred values are standard errors of prediction
  (SEPs) and take into account not only the parameter uncertainty
  associated with the model coefficients (also covered by se.fit), but
  also the random error associated with any given observation (the
  epsilon term in a typical regression model).

- interval:

  character specifying the type of interval to include, if any.
  Prediction intervals are only available when \`agg.by=="unit"\`, and
  confidence intervals are not available for any loadInterp predictions.
  If "prediction", the interval bounds will be returned in columns named
  "lwr.pred" and "upr.pred". Prediction bounds describe the expected
  distribution of observations at each prediction point. The extent of
  the

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

Returns a vector or data.frame of predictions preditions. The result is
a vector if interval is "none" and all of se.fit, se.pred, date, and
count are FALSE; otherwise, the result is a data.frame.

## Details

loadInterps are currently assumed to have normally distributed
residuals. An unwitting user might violate this assumption without being
notified by the code, so be careful! This assumption is mainly relevant
to the calculation of prediction intervals. Also, where other models
such as loadReg and loadLm will retransform predictions back into linear
space, loadInterps will not.

## See also

Other predictSolute:
[`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md),
[`predictSolute.loadComp()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadComp.md),
[`predictSolute.loadLm()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadLm.md),
[`predictSolute.loadModel()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadModel.md),
[`predictSolute.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadReg2.md)
