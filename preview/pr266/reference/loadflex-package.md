# Models and Tools for Watershed Flux Estimates

loadflex implements several of the most common methods for modeling and
predicting watershed solute fluxes and concentrations, including
interpolation and regression models, period-weighted averaging, and the
composite method. Integrates seamlessly with the USGS's rloadest package
and with native R regression models. Offers a uniform interface for any
model type, with which you can quickly fit models, generate predictions,
and aggregate to monthly or annual values.

## Vignettes

- \`vignette('intro_to_loadflex', package='loadflex')\`

## Models

- Linear and period-weighted interpolations:
  [`loadInterp`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md)

- Generic regression-based models:
  [`loadModel`](http://doi-usgs.github.io/loadflex/reference/loadModel.md)

- Regression-based models with the \`rloadest\` package:
  [`loadReg2`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)

- Composite-method-based models:
  [`loadComp`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)

## Predictions

- Make predictions at the resolution of the predictor data or over
  longer time periods:
  [`predictSolute`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)

## Maintenance

Project funding has ended and our maintenance time is limited, but we do
attempt to provide bug fixes and lightweight support as we are able.
Submit questions or suggestions to
https://github.com/DOI-USGS/loadflex/issues.

## See also

Useful links:

- <https://github.com/DOI-USGS/loadflex>

- <http://doi-usgs.github.io/loadflex/>

## Author

**Maintainer**: Alison P. Appling <aappling@usgs.gov>

Authors:

- Alison P. Appling <aappling@usgs.gov>

- Miguel C. Leon

Other contributors:

- Lindsay R. Carr \[contributor\]

- W. David Watkins \[contributor\]
