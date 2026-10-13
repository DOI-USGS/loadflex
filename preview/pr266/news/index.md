# Changelog

## Version 1.9.4

- Adapted `convertToEGRET` to the updated
  [`EGRET::populateDaily()`](https://rdrr.io/pkg/EGRET/man/populateDaily.html),
  which now returns one row per day. The EGRET `Daily` frame is now
  keyed on `Date` (the internal `dateTime` column was removed),
  [`convertToEGRETDaily()`](http://doi-usgs.github.io/loadflex/reference/convertToEGRETDaily.md)
  and
  [`expandFlowForEGRET()`](http://doi-usgs.github.io/loadflex/reference/expandFlowForEGRET.md)
  reject sub-daily flow data with an informative error, and the sample
  flow path computes an instantaneous `Q` per observation rather than
  routing through `populateDaily()`. Date conversions use each series’
  own timezone for consistent `Date` keys.

- Fixed inconsistent prediction column naming for `loadLm`:
  [`predictSolute()`](http://doi-usgs.github.io/loadflex/reference/predictSolute.md)
  now names the prediction column `conc` or `flux` to match the other
  load models, rather than `fit`
  ([\#258](https://github.com/DOI-USGS/loadflex/issues/258)).
