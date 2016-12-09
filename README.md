## `loadflex`: Models and tools for watershed flux estimates

This package implements several of the most common methods for modeling and
predicting watershed solute fluxes and concentrations, including interpolation
and regression models, period-weighted averaging, and the composite method.
loadflex integrates seamlessly with the USGS's
[rloadest](https://github.com/USGS-R/rloadest) package and with native R
regression models. It offers a uniform interface for any model type, with which
you can quickly fit models, generate predictions, and aggregate to monthly or
annual values.

The package was created by [Alison Appling](https://github.com/aappling-usgs) 
and [Miguel Leon](https://github.com/miguelcleon) with the support of [Bill 
McDowell](https://colsa.unh.edu/faculty/mcdowell) and the [McDowell 
lab](http://wrrc.unh.edu/mcdowell-lab-current) at the University of New 
Hampshire, 2013-2015. Funding for the project was provided by the National 
Science Foundation, USDA National Institute of Food and Agriculture, and the NH 
Agricultural Experiment Station. Funding for the example dataset from the 
Lamprey River was provied by the EPA, NH Water Resources Research Center, NH 
Agricultural Experiment Station, NH Sea Grant, USGS, and NSF. This work is 
described in

> Appling, A. P., M. C. Leon, and W. H. McDowell. 2015. Reducing bias and quantifying uncertainty in watershed flux estimates: the R package loadflex. Ecosphere 6(12):269. http://dx.doi.org/10.1890/ES14-00517.1

Additional development and maintenance in 2016-present is being done by 
[Alison Appling](https://github.com/aappling-usgs), [Lindsay
Carr](https://github.com/lindsaycarr), and [David 
Watkins](https://github.com/wdwatkins) of the [USGS Office of Water 
Information](http://cida.usgs.gov/datascience.html).

To see how to cite this package, type `citation("loadflex")`.

To get started, load the package and type `?loadflex`.


## Installation

### First-time installation

loadflex makes use of other packages. To install those packages from scratch, 
run the following lines:

```{r}
install.packages(
  c("smwrData", "smwrBase", "smwrGraphs", "smwrStats", "smwrQW", "rloadest", "unitted"), 
  repos=c("https://owi.usgs.gov/R", "https://cran.rstudio.com"), 
  dependencies=TRUE, type="both")
install.packages(
  c("car", "dplyr", "ggplot2", "lubridate", "MASS", "Matrix"),
  dependencies=TRUE, type="both")
```

You'll also need the `devtools` package; see 
https://www.rstudio.com/products/rpackages/devtools/ for special instructions, 
and also run this command:
```r
install.packages("devtools")
```

and lastly run this call to actually install `loadflex`:
```r
devtools::install_github("USGS-R/loadflex")
```

Also please see the installation FAQ on the wiki
(https://github.com/USGS-R/loadflex/wiki/Installation-FAQ) if you run into
trouble.

### Updates

After the first-time installation, you can update with these commands:
```r
update.packages(
  oldPkgs=c("smwrData", "smwrBase", "smwrGraphs", "smwrStats", "smwrQW", "rloadest", "unitted"),
  dependencies=TRUE, repos=c("https://owi.usgs.gov/R", "https://cran.rstudio.com"))
update.packages(
  oldPkgs=c("car", "dplyr", "ggplot2", "lubridate", "MASS", "Matrix"),
  dependencies=TRUE, type="both")
devtools::install_github("USGS-R/loadflex")
```

## Package status

Please don't worry: yes, some of these status badges are red, but the reasons are minor. You can absolutely still install and run the package. We use the badges as a motivator for us - we're continously raising the bar and working to make `loadflex` even better.

| Name       | Status           |  
| :------------ |:-------------|  
| Windows Build: | [![Windows Build status](https://ci.appveyor.com/api/projects/status/8cjo5urmkv5sjd7v?svg=true)](https://ci.appveyor.com/project/appling/loadflex) |
| Linux Build: | [![Linux Build Status](https://travis-ci.org/USGS-R/loadflex.svg)](https://travis-ci.org/USGS-R/loadflex)  |
| Coveralls: | [![Coveralls Coverage](https://coveralls.io/repos/USGS-R/loadflex/badge.svg?branch=master)](https://coveralls.io/r/USGS-R/loadflex?branch=master) |
| Codecov: | [![Codecov Coverage](https://codecov.io/github/USGS-R/loadflex/coverage.svg?branch=master)](https://codecov.io/github/USGS-R/loadflex?branch=master) |
