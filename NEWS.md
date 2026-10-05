# 1.9.3

* Fixed the grouping in `aggregateSolute` by replacing the defunct dplyr
`group_by_()` with `group_by(across(all_of(...)))`.

* Fixed the column selection and renaming in `convertToEGRET` by replacing the
defunct dplyr `select_()` and `rename_()` with `select()` and `rename()` using
`all_of()`.


# 1.2.0 - 1.9.2

* `aggregateSolute` is no longer exported and is now an internal function. The
aggregation workflow it provided is now reached through the `agg.by` argument of
`predictSolute`. The `vignettes/intro_to_loadflex.Rmd` vignette was updated to
explain this change and demonstrate the new API.

* Dropped support for `format = "flux total"` in `aggregateSolute`. It was first
deprecated with a warning and is now unsupported; multiply a flux rate by its
duration to obtain total flux.

* Made several `aggregateSolute` arguments defunct: `se.preds`, `ci.agg`,
`deg.free`, `ci.distrib`, `se.agg`, and `cormat.function`. These are now
absorbed into `...` and trigger a warning if supplied. Related aggregation
options `"mean water year"` and `"mean calendar year"` were also removed, and
aggregate uncertainty estimates (`SE`, `CI_lower`, `CI_upper`) now return `NA`
because the earlier estimates were unreliable.


# 1.0.2 - 1.1.11

* New function: `plotEGRET`. Generates plots of loadflex inputs and outputs 
using code already written and refined in the EGRET load estimation package.

* New function: `summarizeModel`. Available for all loadModel classes.

* Expanded list of fields available in `metadata` class, for improved plotting
and summarization.

* Lightly altered the arguments and features of `getUnits` and `getInfo`.


# 1.0.1

* This version is consistent with the description in Appling, A. P., M. C. Leon,
and W. H. McDowell. 2015. Reducing bias and quantifying uncertainty in watershed
flux estimates: the R package loadflex. Ecosphere 6(12):269. 
https://doi.org/10.1890/ES14-00517.1.
