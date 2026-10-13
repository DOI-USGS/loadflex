# Create a fitted loadLm object.

Generates a new model of class loadInterp
([`loadLm-class`](http://doi-usgs.github.io/loadflex/reference/loadLm-class.md))
which can connect observations of concentration or flux using a linear
regression model.

## Usage

``` r
loadLm(
  formula,
  pred.format = c("flux", "conc"),
  data,
  metadata,
  fitting_function = NULL,
  y.trans.function = NULL,
  retrans.function = exp,
  store = c("data", "fitting.function"),
  ylog
)
```

## Arguments

- formula:

  A formula specifying the linear model to fit.

- pred.format:

  In what format (flux or conc) does the lm object make predictions when
  predict.lm() is called?

- data:

  data.frame. The data to be interpolated

- metadata:

  metadata, used to access the appropriate columns of data. At a
  minimum, `metadata` should correctly specify the date column.

- fitting_function:

  a fitting function that can produce a new loadComp object from new
  data (this currently uses the same new data for both regression
  calibration and interpolation)

- y.trans.function:

  function that accepts a vector of predictions straight from the fitted
  model and retransforms them, if appropriate, to predictions in linear
  space and with units of concentration\*flow. Because load models are
  frequently fit to log(y) ~ ..., the default is retrans.function=exp.
  After retrans.function has been applied to the predictions, the
  retransformed predictions will automatically undergo further units
  conversion from conc\*flow to load.rate, according to the metadata
  elements conc.units, flow.units, and load.rate.units.

- retrans.function:

  function that accepts a vector of predictions straight from the fitted
  model and retransforms them, if appropriate, to predictions in linear
  space and with units of concentration\*flow. Because load models are
  frequently fit to log(y) ~ ..., the default is retrans.function=exp.
  After retrans.function has been applied to the predictions, the
  retransformed predictions will automatically undergo further units
  conversion from conc\*flow to load.rate, according to the metadata
  elements conc.units, flow.units, and load.rate.units.

- store:

  One or more character strings specifying which information to write
  within the model. Options are 'data': the original fitting data;
  'fitting.function': a fitting function that can produce a new loadComp
  object from new data

- ylog:

  logical. If TRUE, this constitutes affirmation that the values passed
  to the left-hand side of the model formula will be in log space. If
  missing, the value of `ylog` will be inferred from the values of
  `formula` and `y.trans.function`, but be warned that this inference is
  fallible.

## Value

A fitted loadLm model.

## See also

Other load.model.inits:
[`loadComp()`](http://doi-usgs.github.io/loadflex/reference/loadComp.md),
[`loadInterp()`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadModel()`](http://doi-usgs.github.io/loadflex/reference/loadModel.md),
[`loadReg2()`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)

## Examples

``` r
data(eg_fitdat, eg_metadata)
no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE) + DATE, pred.format="conc",
  data=eg_fitdat, metadata=eg_metadata, retrans=exp)
```
