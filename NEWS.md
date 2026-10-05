# 1.9.3

* Fixed the grouping in `aggregateSolute` by replacing the defunct dplyr
`group_by_()` with `group_by(across(all_of(...)))`.

* Fixed the column selection and renaming in `convertToEGRET` by replacing the
defunct dplyr `select_()` and `rename_()` with `select()` and `rename()` using
`all_of()`.


# 1.1.0 - 1.1.20 or so

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
