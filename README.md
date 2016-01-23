## `loadflex`: Models and tools for watershed flux estimates

This package implements several of the most common methods for 
modeling and predicting watershed solute fluxes and concentrations, including
interpolation and regression models, period-weighted averaging, and the
composite method. loadflex integrates seamlessly with the USGS's rloadest 
package and with native R regression models. It offers a uniform interface
for any model type, with which you can quickly fit models, generate
predictions, and aggregate to monthly or annual values.

The package was developed by Alison Appling and Miguel Leon with the support 
of Bill McDowell at the University of New Hampshire.

To get started, load the package and type `?loadflex`.

To see how to cite this package, type `citation("loadflex")`.


## Installation

loadflex makes use of packages that are currently only available from 
GitHub or the USGS R package repository. To install these packages, 
run the following lines:

```{r}
install.packages(c("smwrData", "smwrBase", "smwrGraphs", "smwrStats", 
"smwrQW", "rloadest"), repos=c("http://owi.usgs.gov/R", 
"http://cran.us.r-project.org"), dependencies=TRUE, type="both")

install.packages("devtools")
# also see https://www.rstudio.com/products/rpackages/devtools/

devtools::install_github(c("appling/unitted", "mcdowelllab/loadflex"))
```

Also please see the installation FAQ on the wiki (https://github.com/McDowellLab/loadflex/wiki/Installation-FAQ) if you run into trouble.

## Package status

Please don't worry: yes, some of these status badges are red, but the reasons are minor. You can absolutely still install and run the package. We use the badges as a motivator for us - we're continously raising the bar and working to make `loadflex` even better.

| Name       | Status           |  
| :------------ |:-------------|  
| Windows Build: | [![Windows Build status](https://ci.appveyor.com/api/projects/status/8cjo5urmkv5sjd7v?svg=true)](https://ci.appveyor.com/project/appling/loadflex) |
| Linux Build: | [![Linux Build Status](https://travis-ci.org/McDowellLab/loadflex.svg)](https://travis-ci.org/McDowellLab/loadflex)  |
| Coveralls: | [![Coveralls Coverage](https://coveralls.io/repos/McDowellLab/loadflex/badge.svg?branch=master)](https://coveralls.io/r/McDowellLab/loadflex?branch=master) |
| Codecov: | [![Codecov Coverage](https://img.shields.io/codecov/c/github/mcdowelllab/loadflex.svg)](https://codecov.io/github/mcdowelllab/loadflex) |

