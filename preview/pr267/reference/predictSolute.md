# Make flux or concentration predictions from a load model.

A function in the loadModelInterface. Uses a load model and a predictor
dataset (which may differ from the original model-fitting dataset) to
make predictions for loads or concentrations at the time points in the
new dataset.

## Usage

``` r
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

  A load model object, probably a loadInterp, loadReg2, loadComp, or
  loadLm. The object should typically inherit from the loadModel class
  and must always implement the loadModelInterface.

- flux.or.conc:

  character. Should the predictions be reported as flux rates or
  concentrations? If the output is a data.frame, the column name for
  flux predictions will be "fit" when \`agg.by='unit'\` and "Flux_Rate"
  otherwise; the column name for concentration predictions will be "fit"
  when \`agg.by='unit'\` and "Conc" otherwise.

- newdata:

  An optional data.frame of predictor observations. The column names in
  this data.frame must match those specified in the load model's
  metadata. If `newdata` is not supplied, the original fitting data will
  be used.

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

If interval=="none" and all of dates, se.fit, se.pred, and count are
FALSE, returns a vector of predictions. Otherwise, returns a data.frame.
If agg.by=="unit" then the data.frame will have a column called "flux"
or "conc" containing the predictions for the solute, and optional
columns associated with datetimes, interval, se.fit, and se.pred are
additional columns with names noted in those argument descriptions. If
\`agg.by!="unit"\`, the returned names of the grouping column or columns
will reflect the selected value of \`agg.by\` (e.g., 'month').

## Details

This is the S3 generic predictSolute(), for which specific methods
should be implemented for each load model class (e.g.,
[`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md).
Unlike rloadest::predLoad() and predConc(), and more like most other
predict functions in R, this function makes no attempt to aggregate the
results.

## See also

Other loadModelInterface:
[`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md),
[`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md),
[`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md),
[`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md),
[`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md),
[`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md),
[`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md),
[`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)

Other predictSolute:
[`predictSolute.loadComp()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadComp.md),
[`predictSolute.loadInterp()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadInterp.md),
[`predictSolute.loadLm()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadLm.md),
[`predictSolute.loadModel()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadModel.md),
[`predictSolute.loadReg2()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadReg2.md)
