# A generic load model class.

Class and function definitions for very generic load models that you can
create with a single function call
([`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md))
or extend to more specific model types (e.g.,
[`loadInterp`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md),
[`loadReg2`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md),
[`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md))

## Slots

- `fit`:

  A statistical model, fit to the data and wrapped by the loadModel
  class for additional functionality specific to load models.

- `pred.format`:

  A string indicating the format of predictions (flux or conc).

- `metadata`:

  A metadata object describing the load model.

- `data`:

  The fitting data for the model (fit).

- `fitting.function`:

  The function used to create or recreate the loadModel, possibly with
  new fitting data.

- `y.trans.function`:

  A function to be applied to the y variable before fitting the model.

- `retrans.function`:

  A function to be applied to the y predictions before returning their
  values from
  [`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md).

## See also

Other load.model.classes:
[`loadComp-class`](http://doi-usgs.github.io/loadflex/reference/loadComp-class.md),
[`loadInterp-class`](http://doi-usgs.github.io/loadflex/reference/loadInterp-class.md),
[`loadLm-class`](http://doi-usgs.github.io/loadflex/reference/loadLm-class.md),
[`loadReg2-class`](http://doi-usgs.github.io/loadflex/reference/loadReg2-class.md)
