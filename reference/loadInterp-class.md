# A load model class specific to interpolations for flux estimation.

loadInterps use a variety of interpolation methods to connect
predictions of y values (usually fluxes, concentrations, or residuals)
over time.

## Slots

- `fit`:

  the interpolation model to be used.

- `MSE`:

  numeric. The mean squared error, i.e., the variance of prediction
  errors, probably as estimated by leave-one-out cross validation.

## See also

Other load.model.classes:
[`loadComp-class`](http://doi-usgs.github.io/loadflex/reference/loadComp-class.md),
[`loadLm-class`](http://doi-usgs.github.io/loadflex/reference/loadLm-class.md),
[`loadModel-class`](http://doi-usgs.github.io/loadflex/reference/loadModel-class.md),
[`loadReg2-class`](http://doi-usgs.github.io/loadflex/reference/loadReg2-class.md)
