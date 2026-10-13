# Package index

## All functions

- [`aggregateSolute()`](http://doi-usgs.github.io/loadflex/reference/aggregateSolute.md)
  : Aggregate loads by the time periods specified by the user

- [`as.data.frame(`*`<metadata>`*`)`](http://doi-usgs.github.io/loadflex/reference/as.data.frame.metadata.md)
  : Convert a metadata object to a 1-row data.frame

- [`compModel-class`](http://doi-usgs.github.io/loadflex/reference/compModel-class.md)
  : \#### compModel class \#### The engine of a loadInterp model.

- [`convertToEGRET()`](http://doi-usgs.github.io/loadflex/reference/convertToEGRET.md)
  : Convert loadflex to EGRET object

- [`convertToEGRETDaily()`](http://doi-usgs.github.io/loadflex/reference/convertToEGRETDaily.md)
  : Convert estimation and load prediction data into the EGRET Daily
  data.frame

- [`convertToEGRETInfo()`](http://doi-usgs.github.io/loadflex/reference/convertToEGRETInfo.md)
  : Convert a loadflex metadata object into the EGRET INFO dataframe.

- [`convertToEGRETSample()`](http://doi-usgs.github.io/loadflex/reference/convertToEGRETSample.md)
  : Convert the interpolation data.frame into the EGRET Sample
  dataframe.

- [`convertUnits()`](http://doi-usgs.github.io/loadflex/reference/convertUnits.md)
  : Produce conversion factor to multiply old.units by to get new.units

- [`rhoEqualDates()`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)
  [`rho1DayBand()`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)
  : Get the assumed correlation between residuals or predictions at
  pairs of dates.

- [`cormatEqualDates()`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)
  [`cormat1DayBand()`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)
  [`cormatDiagonal()`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)
  : Functions that each produce an autocorrelation matrix with a
  specified pattern.

- [`correlations`](http://doi-usgs.github.io/loadflex/reference/correlations.md)
  :

  Correlation functions in loadflex

- [`dimInfo()`](http://doi-usgs.github.io/loadflex/reference/dimInfo.md)
  : Return a data.frame describing each dimension of the units (1 row
  per dimension)

- [`eg_loadflex`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`lamprey_discharge`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`lamprey_nitrate`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_fitdat`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_estdat`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_metadata`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_loadInterp`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_loadLm`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_loadReg2`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  [`eg_loadComp`](http://doi-usgs.github.io/loadflex/reference/eg_loadflex.md)
  :

  Example datasets and objects for the loadflex package

- [`` `==`( ``*`<metadata>`*`,`*`<metadata>`*`)`](http://doi-usgs.github.io/loadflex/reference/equals.metadata.md)
  : Basic equality test for two metadata objects.

- [`estimateMSE()`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.md)
  : Estimate model uncertainty algorithmically.

- [`estimateMSE(`*`<loadComp>`*`)`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadComp.md)
  : Estimate uncertainty in a composite model.

- [`estimateMSE(`*`<loadInterp>`*`)`](http://doi-usgs.github.io/loadflex/reference/estimateMSE.loadInterp.md)
  : Estimate uncertainty in an interpolation using leave-one-out cross
  validation.

- [`estimateRho()`](http://doi-usgs.github.io/loadflex/reference/estimateRho.md)
  : Estimate the autocorrelation of a mid- to high-resolution time
  series

- [`expandFlowForEGRET()`](http://doi-usgs.github.io/loadflex/reference/expandFlowForEGRET.md)
  : Convert a date and discharge data.frame into EGRET format

- [`flowconcToFluxConversion()`](http://doi-usgs.github.io/loadflex/reference/flowconcToFluxConversion.md)
  : Provide the conversion factor which, when multiplied by flow \*
  conc, gives the flux in the desired units

- [`formatPreds()`](http://doi-usgs.github.io/loadflex/reference/formatPreds.md)
  : formatPreds raw to final predictions

- [`genericDistanceWeightedInterpolation()`](http://doi-usgs.github.io/loadflex/reference/genericDistanceWeightedInterpolation.md)
  : A parameterizable distance-weighted interpolation function.

- [`genericSmoothSplineInterpolation()`](http://doi-usgs.github.io/loadflex/reference/genericSmoothSplineInterpolation.md)
  : A parameterizable smoothing spline function.

- [`genericTriangularInterpolation()`](http://doi-usgs.github.io/loadflex/reference/genericTriangularInterpolation.md)
  : A parameterizable triangular interpolation function.

- [`getCormatCustom()`](http://doi-usgs.github.io/loadflex/reference/getCormatCustom.md)
  : Turn an autocorrelation function into a function that produces a
  correlation matrix

- [`getCormatFirstOrder()`](http://doi-usgs.github.io/loadflex/reference/getCormatFirstOrder.md)
  : get rho matrix first order

- [`getCormatTaoBand()`](http://doi-usgs.github.io/loadflex/reference/getCormatTaoBand.md)
  : Same idea as getCormatCustom(rho1DayBand, dates) but runs faster.

- [`getCorrectionFraction()`](http://doi-usgs.github.io/loadflex/reference/getCorrectionFraction.md)
  : The fraction of prediction that is due to a correction.

- [`getDistanceWeightedInterpolation()`](http://doi-usgs.github.io/loadflex/reference/getDistanceWeightedInterpolation.md)
  : Generate a distance-weighted interpolation function with the
  parameters of your choice.

- [`getFittedModel()`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.md)
  : Retrieve the fitted model, if appropriate, from a loadModel load
  model

- [`getFittedModel(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/getFittedModel.loadModel.md)
  : Retrieve the fitted model, if appropriate, from a loadModel load
  model

- [`getFittingData()`](http://doi-usgs.github.io/loadflex/reference/getFittingData.md)
  : Extract the data originally used to fit a load model.

- [`getFittingData(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/getFittingData.loadModel.md)
  : Retrieve the data used to fit the model

- [`getFittingFunction()`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.md)
  : Get a function that can be used to refit the load.model with new
  data.

- [`getFittingFunction(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/getFittingFunction.loadModel.md)
  : Retrieve a fitting function from a loadModel load model

- [`getMetadata()`](http://doi-usgs.github.io/loadflex/reference/getMetadata.md)
  : Extract metadata from a load model.

- [`getMetadata(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/getMetadata.loadModel.md)
  : Retrieve metadata from a loadModel load model

- [`getMetadata(`*`<loadReg>`*`)`](http://doi-usgs.github.io/loadflex/reference/getMetadata.loadReg.md)
  : Extracts and imports metadata from an rloadest loadReg model into an
  object of class "metadata"

- [`getResiduals()`](http://doi-usgs.github.io/loadflex/reference/getResiduals.md)
  : getResiduals return the residuals of the load.model

- [`getRhoFirstOrderFun()`](http://doi-usgs.github.io/loadflex/reference/getRhoFirstOrderFun.md)
  : Produces a function that uses a first-order autocorrelation model to
  estimate the correlation between two dates.

- [`getSmoothSplineInterpolation()`](http://doi-usgs.github.io/loadflex/reference/getSmoothSplineInterpolation.md)
  : Generate a smoothing spline function with the parameters of your
  choice.

- [`getTriangularInterpolation()`](http://doi-usgs.github.io/loadflex/reference/getTriangularInterpolation.md)
  : Generate a triangular interpolation function with the parameters of
  your choice.

- [`interpModel-class`](http://doi-usgs.github.io/loadflex/reference/interpModel-class.md)
  : loadInterp is a class of load models that hold interpolation
  functions. The engine of a loadInterp model.

- [`linearInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  [`triangularInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  [`rectangularInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  [`splineInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  [`smoothSplineInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  [`distanceWeightedInterpolation()`](http://doi-usgs.github.io/loadflex/reference/interpolations.md)
  : Interpolation functions

- [`isTimestepRegular()`](http://doi-usgs.github.io/loadflex/reference/isTimestepRegular.md)
  : Check a time series for evenly spaced dates.

- [`loadComp-class`](http://doi-usgs.github.io/loadflex/reference/loadComp-class.md)
  : A load model class implementing the composite method for flux
  estimation.

- [`loadComp()`](http://doi-usgs.github.io/loadflex/reference/loadComp.md)
  : Create a fitted loadComp object.

- [`loadInterp-class`](http://doi-usgs.github.io/loadflex/reference/loadInterp-class.md)
  : A load model class specific to interpolations for flux estimation.

- [`loadInterp()`](http://doi-usgs.github.io/loadflex/reference/loadInterp.md)
  : Create a fitted loadInterp object.

- [`loadLm-class`](http://doi-usgs.github.io/loadflex/reference/loadLm-class.md)
  :

  A load model class specific to simple linear models
  ([`lm`](https://rdrr.io/r/stats/lm.html)s) for flux estimation.

- [`loadLm()`](http://doi-usgs.github.io/loadflex/reference/loadLm.md) :
  Create a fitted loadLm object.

- [`loadModel-class`](http://doi-usgs.github.io/loadflex/reference/loadModel-class.md)
  : A generic load model class.

- [`loadModel()`](http://doi-usgs.github.io/loadflex/reference/loadModel.md)
  : Create a fitted loadModel object.

- [`loadModelInterface`](http://doi-usgs.github.io/loadflex/reference/loadModelInterface.md)
  :

  Functions implemented by any `loadflex`-compatible load model.

- [`loadReg2-class`](http://doi-usgs.github.io/loadflex/reference/loadReg2-class.md)
  : A load model class specific to loadReg objects produced by the USGS
  rloadest package.

- [`loadReg2()`](http://doi-usgs.github.io/loadflex/reference/loadReg2.md)
  : Create a fitted loadReg2 object.

- [`loadflex-deprecated-data`](http://doi-usgs.github.io/loadflex/reference/loadflex-deprecated-data.md)
  :

  Renamed or deprecated datasets for the loadflex package

- [`exampleMetadata()`](http://doi-usgs.github.io/loadflex/reference/loadflex-deprecated.md)
  :

  Deprecated functions in the loadflex package

- [`loadflex`](http://doi-usgs.github.io/loadflex/reference/loadflex.md)
  : Models and Tools for Watershed Flux Estimates

- [`logToLin()`](http://doi-usgs.github.io/loadflex/reference/lognormal-moments.md)
  [`linToLog()`](http://doi-usgs.github.io/loadflex/reference/lognormal-moments.md)
  [`mixedToLog()`](http://doi-usgs.github.io/loadflex/reference/lognormal-moments.md)
  : Translate means and standard errors/deviations of lognormal
  distributions between log and linear space.

- [`match.arg.loadflex()`](http://doi-usgs.github.io/loadflex/reference/match.arg.loadflex.md)
  : Require an argument to match the loadflex conventions for that
  argument name

- [`metadata-class`](http://doi-usgs.github.io/loadflex/reference/metadata-class.md)
  : Store metadata relevant to a load model.

- [`getCol()`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md)
  [`getUnits()`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md)
  [`getInfo()`](http://doi-usgs.github.io/loadflex/reference/metadata-getters.md)
  : Access information about a load model.

- [`metadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)
  [`updateMetadata()`](http://doi-usgs.github.io/loadflex/reference/metadata.md)
  : Create or modify the metadata for a load model.

- [`observeSolute()`](http://doi-usgs.github.io/loadflex/reference/observeSolute.md)
  : observeSolute - instantaneous loads or concentrations

- [`plotEGRET()`](http://doi-usgs.github.io/loadflex/reference/plotEGRET.md)
  : Create an EGRET-style plot

- [`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)
  : Make flux or concentration predictions from a load model.

- [`predictSolute(`*`<loadComp>`*`)`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadComp.md)
  : Make flux or concentration predictions from a loadComp model.

- [`predictSolute(`*`<loadInterp>`*`)`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadInterp.md)
  : Make flux or concentration predictions from a loadInterp model.

- [`predictSolute(`*`<loadLm>`*`)`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadLm.md)
  : Make flux or concentration predictions from a loadLm model.

- [`predictSolute(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadModel.md)
  : Make flux or concentration predictions from a loadModel model.

- [`predictSolute(`*`<loadReg2>`*`)`](http://doi-usgs.github.io/loadflex/reference/predictSolute.loadReg2.md)
  : Make flux or concentration predictions from a loadReg2 model.

- [`resampleCoefficients.lm()`](http://doi-usgs.github.io/loadflex/reference/resampleCoefficients.lm.md)
  : Resample the coefficients of a linear model (lm)

- [`resampleCoefficients.loadReg()`](http://doi-usgs.github.io/loadflex/reference/resampleCoefficients.loadReg.md)
  : Resample the coefficients from a loadReg model.

- [`residDurbinWatson()`](http://doi-usgs.github.io/loadflex/reference/residDurbinWatson.md)
  : Test for autocorrelation of residuals

- [`show(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/show.loadModel.md)
  : Display a loadModel object

- [`show(`*`<metadata>`*`)`](http://doi-usgs.github.io/loadflex/reference/show.metadata.md)
  : Display a metadata object

- [`simulateSolute()`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.md)
  : Simulate solute concentrations based on the model and model
  uncertainty.

- [`simulateSolute(`*`<loadLm>`*`)`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadLm.md)
  : Produce a set of predictions that reflect the coefficient
  uncertainty and natural variation.

- [`simulateSolute(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadModel.md)
  : Produce a set of predictions that reflect the coefficient
  uncertainty and natural variation.

- [`simulateSolute(`*`<loadReg2>`*`)`](http://doi-usgs.github.io/loadflex/reference/simulateSolute.loadReg2.md)
  : Produce a set of predictions that reflect the coefficient
  uncertainty and possibly also natural variation.

- [`summarizeInputs()`](http://doi-usgs.github.io/loadflex/reference/summarizeInputs.md)
  : Summarize the site and input data

- [`summarizeModel()`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.md)
  : Extract model summary statistics

- [`summarizeModel(`*`<loadComp>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadComp.md)
  : Extract model summary statistics from a loadComp model

- [`summarizeModel(`*`<loadInterp>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadInterp.md)
  : Extract model summary statistics from a loadInterp model

- [`summarizeModel(`*`<loadLm>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadLm.md)
  : Extract model summary statistics from a loadLm model

- [`summarizeModel(`*`<loadModel>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadModel.md)
  : Extract model summary statistics from a loadModel model

- [`summarizeModel(`*`<loadReg>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg.md)
  :

  Extract model summary statistics from an
  [`rloadest::loadReg()`](https://rdrr.io/pkg/rloadest/man/loadReg.html)
  model

- [`summarizeModel(`*`<loadReg2>`*`)`](http://doi-usgs.github.io/loadflex/reference/summarizeModel.loadReg2.md)
  : Extract model summary statistics from a loadReg2 model

- [`translateFreeformToUnitted()`](http://doi-usgs.github.io/loadflex/reference/translateFreeformToUnitted.md)
  : Convert units from a greater variety of forms, including rloadest
  form, to unitted form

- [`unitType()`](http://doi-usgs.github.io/loadflex/reference/unitType.md)
  : Return the unit.type of a unit string

- [`units_loadflex`](http://doi-usgs.github.io/loadflex/reference/units_loadflex.md)
  [`valid.metadata.units`](http://doi-usgs.github.io/loadflex/reference/units_loadflex.md)
  [`freeform.unit.translations`](http://doi-usgs.github.io/loadflex/reference/units_loadflex.md)
  [`unit.conversions`](http://doi-usgs.github.io/loadflex/reference/units_loadflex.md)
  :

  Units-related datasets for the loadflex package

- [`validDim()`](http://doi-usgs.github.io/loadflex/reference/validDim.md)
  : Determine whether a 1D dimension string is known and of the expected
  dimension type

- [`validLoadModelInterface()`](http://doi-usgs.github.io/loadflex/reference/validLoadModelInterface.md)
  : Test whether a class implements the loadModelInterface

- [`validMetadataUnits()`](http://doi-usgs.github.io/loadflex/reference/validMetadataUnits.md)
  : Check whether these units are acceptable (without translation) for
  inclusion in metadata
