# Create a fitted loadModel object.

Generates a new model of class loadModel
([`loadModel-class`](http://doi-usgs.github.io/loadflex/reference/loadModel-class.md)).

## Usage

``` r
loadModel(
  inner.fit.function,
  pred.format,
  data,
  metadata,
  y.trans.function = NULL,
  retrans.function = exp,
  store = c("data", "fitting.function")
)
```

## Arguments

- inner.fit.function:

  function that accepts one argument, a training data frame, and returns
  a \[re\]fitted statistical model, such as an `lm`, relating flux or
  concentration to predictors in the training data.

- pred.format:

  character string that describes the output of
  retrans.function(predict(fit)) \[NOT of predictSolute.loadModel(),
  which can do either!\]

- data:

  data.frame containing the initial training observations

- metadata:

  An object of class
  [`metadata`](http://doi-usgs.github.io/loadflex/reference/metadata.md)

- y.trans.function:

  function that accepts a vector of observed response values (e.g.,
  concentrations or flux rates) and transforms them into the values on
  the left-hand side of the calibration formula. Because load models are
  frequently fit to log(y) ~ ..., the default is y.trans.function=log.

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

## Value

A fitted loadModel.

## See also

Other load.model.inits:
[`loadComp()`](http://doi-usgs.github.io/loadflex/reference/loadComp.md),
[`loadInterp()`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadLm()`](http://doi-usgs.github.io/loadflex/reference/loadLm.md),
[`loadReg2()`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)
