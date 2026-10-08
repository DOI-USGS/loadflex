# loadInterp is a class of load models that hold interpolation functions. The engine of a loadInterp model.

This is a model class to nest within loadInterp. This class,
interpModel, is lightweight relative to loadInterp: its focus is on the
interpolation rather than on units conversions, fitting, or other things
that loadInterp does better. This is also the class that is produced by
a call to the fitting function stored in loadInterp.

## Slots

- `dates.class`:

  Class of the dates.in.

- `dates.in`:

  Dates of the y.in data.

- `y.in`:

  Data (usually fluxes, concentrations, or residuals) to interpolate.

- `interp.function`:

  Function that accepts arguments `dates.in`, `y.in`, and `dates.out`,
  then returns predictions `y.out`

## See also

Other load.model.fits:
[`compModel-class`](http://doi-usgs.github.io/loadflex/reference/compModel-class.md)
