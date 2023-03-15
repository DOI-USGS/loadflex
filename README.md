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

> Appling, A. P., M. C. Leon, and W. H. McDowell. 2015. Reducing bias and quantifying uncertainty in watershed flux estimates: the R package loadflex. Ecosphere 6(12):269. https://doi.org/10.1890/ES14-00517.1

Additional development and maintenance in 2016-present is being done by 
[Alison Appling](https://github.com/aappling-usgs), [Lindsay
Carr](https://github.com/lindsaycarr), and [David 
Watkins](https://github.com/wdwatkins) of the [USGS Office of Water 
Information](http://cida.usgs.gov/datascience.html).

To see how to cite this package, type `citation("loadflex")`.

To get started, load the package and type `?loadflex`.

## Package Status

| Name       | Status           |  
| :------------ |:-------------|  
| Windows Build: | [![Windows Build status](https://ci.appveyor.com/api/projects/status/764y0hsh5x3vhufx?svg=true)](https://ci.appveyor.com/project/aappling-usgs/loadflex) |
| Linux Build: | [![Linux Build Status](https://travis-ci.org/USGS-R/loadflex.svg)](https://travis-ci.org/USGS-R/loadflex)  |
| Coveralls: | [![Coveralls Coverage](https://coveralls.io/repos/USGS-R/loadflex/badge.svg?branch=master)](https://coveralls.io/r/USGS-R/loadflex?branch=master) |
| USGS Status: | [![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)|

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/loadflex/issues](https://github.com/USGS-R/loadflex/issues)

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

### Package Support

The Water Mission Area of the USGS supports maintenance of `loadflex` through September 2018, and likely further into the future. Resources are available primarily for maintenance and responding to user questions. Some new features may be added as prioritized by the `loadflex` development team.

[![USGS](http://usgs-r.github.io/images/usgs.png)](https://www.usgs.gov/)

## Installation of loadflex

To install the loadflex package, use the `remotes`
package (running `install.packages('remotes')` first if needed). You will also need a compiler to install `smwrStats`, `smwrQW`, and `rloadest` -- for Windows, see https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html. For Mac, see https://mac.r-project.org/tools/. 

``` r
library(remotes)
remotes::install_gitlab("water/analysis-tools/smwrData", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrBase", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrGraphs", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrStats", host = "code.usgs.gov") # needs compilation
remotes::install_gitlab("water/analysis-tools/smwrQW", host = "code.usgs.gov")    # needs compilation
remotes::install_gitlab("water/analysis-tools/rloadest", host = "code.usgs.gov")  # needs compilation
remotes::install_github("aappling/unitted")
remotes::install_github("USGS-R/loadflex")
```

Also please see the installation FAQ on the wiki
(https://github.com/USGS-R/loadflex/wiki/Installation-FAQ) if you run into trouble.


### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/loadflex/issues](https://github.com/USGS-R/loadflex/issues)

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

### Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/loadflex/blob/master/CONDUCT.md) for more information.

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
