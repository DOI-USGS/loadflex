# A load model class specific to simple linear models ([`lm`](https://rdrr.io/r/stats/lm.html)s) for flux estimation.

loadLms can take any lm formula.

## Slots

- `fit`:

  the interpolation model to be used.

- `ylog`:

  logical. If TRUE, this constitutes affirmation that the values passed
  to the left-hand side of the model formula will be in log space. If
  missing, the value of `ylog` will be inferred from the values of
  `formula` and `y.trans.function`, but be warned that this inference is
  fallible.

## See also

Other load.model.classes:
[`loadComp-class`](http://doi-usgs.github.io/loadflex/reference/loadComp-class.md),
[`loadInterp-class`](http://doi-usgs.github.io/loadflex/reference/loadInterp-class.md),
[`loadModel-class`](http://doi-usgs.github.io/loadflex/reference/loadModel-class.md),
[`loadReg2-class`](http://doi-usgs.github.io/loadflex/reference/loadReg2-class.md)
