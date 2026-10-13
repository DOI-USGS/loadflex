# \#### compModel class \#### The engine of a loadInterp model.

This model class is designed to nest within loadComp objects. The
compModel class is actually a container for two inner models
implementing the loadModel interface: reg.model and resid.model.
reg.model will usually be a regression model, often of type loadReg.
resid.model is typically a loadInterp, used to interpolate among the
residuals of the reg.model predictions.

## Slots

- `reg.model`:

  The regression model - any model that implements the
  loadModelInterface.

- `resid.model`:

  The interpolation model - any model that implements the
  loadModelInterface.

- `log.resids`:

  Should residuals be computed in log space (TRUE) or linear space
  (FALSE)?

- `abs.or.rel.resids`:

  character specifying whether interpolation should be by absolute
  residuals (obs-pred) or relative residuals (obs/pred).

## See also

Other load.model.fits:
[`interpModel-class`](http://doi-usgs.github.io/loadflex/reference/interpModel-class.md)
