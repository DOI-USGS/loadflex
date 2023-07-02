# loadflex: Models and Tools for Watershed Flux Estimates

<span style="color: red;"> In summer or fall 2023, this package will
move from <https://github.com/USGS-R/loadflex> to
<https://github.com/DOI-USGS/loadflex> Please update your links
accordingly. </span>

The `loadflex` R package implements several of the most common methods
for modeling and predicting watershed solute fluxes and concentrations,
including interpolation and regression models, period-weighted
averaging, and the composite method. `loadflex` integrates seamlessly
with the USGS’s
[rloadest](https://code.usgs.gov/water/analysis-tools/rloadest) package
and with native R regression models. It offers a uniform interface for
any model type, with which you can quickly fit models, generate
predictions, and aggregate to monthly or annual values.

This package has been described in Appling et al. (2015):

> Appling, A. P., Leon, M. C., & McDowell, W. H. (2015). Reducing bias
> and quantifying uncertainty in watershed flux estimates: The R package
> `loadflex`. Ecosphere, 6(12), art269.
> <https://doi.org/10.1890/ES14-00517.1>

To see the recommended citation for this package, please run
`citation('loadflex')` at the R prompt.

``` r
citation('loadflex')
## 
## To cite package 'loadflex' in publications use:
## 
##   Appling, A. P., M. C. Leon, and W. H. McDowell. 2015. Reducing bias
##   and quantifying uncertainty in watershed flux estimates: the R
##   package loadflex. Ecosphere 6(12):269.
##   https://doi.org/10.1890/ES14-00517.1
## 
## A BibTeX entry for LaTeX users is
## 
##   @Article{,
##     title = {Reducing bias and quantifying uncertainty in watershed flux estimates: the R package loadflex},
##     author = {Alison P. Appling and Miguel C. Leon and William H. McDowell},
##     year = {2015},
##     journal = {Ecosphere},
##     volume = {6},
##     number = {12},
##     pages = {art269},
##     doi = {10.1890/ES14-00517.1},
##     url = {http://www.esajournals.org/doi/full/10.1890/ES14-00517.1},
##   }
```

## Installation

To install the `loadflex` package, use the `remotes` package as follows
(running `install.packages('remotes')` first if needed). To use
`remotes::install_gitlab()` it is convenient to set a [GitLab Personal
Access Token
(PAT)](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html).
Similarly, to use `remotes::install_github()` it is convenient to set a
[GitHub
PAT](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).
There are [several
methods](https://usethis.r-lib.org/articles/git-credentials.html) for
setting your PATs within R; the simplest is to call
\`Sys.setenv(GITLAB_PAT=“xxxx”, GITHUB_PAT=“yyyy”), replacing xxxx and
yyyy with the PATs you established on the GitLab and GitHub websites.

You will also need a compiler to install `smwrStats`, `smwrQW`, and
`rloadest` – for Windows, see
<https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html>.
For Mac, see <https://mac.r-project.org/tools/>.

``` r
library(remotes)
remotes::install_gitlab("water/analysis-tools/smwrData", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrBase", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrGraphs", host = "code.usgs.gov")
remotes::install_gitlab("water/analysis-tools/smwrStats", host = "code.usgs.gov") # needs compilation
remotes::install_gitlab("water/analysis-tools/smwrQW", host = "code.usgs.gov")    # needs compilation
remotes::install_gitlab("water/analysis-tools/rloadest", host = "code.usgs.gov")  # needs compilation
remotes::install_github("appling/unitted")
remotes::install_github("DOI-USGS/EGRET")
remotes::install_github("USGS-R/loadflex") # soon to be "DOI-USGS/loadflex"
```

Also please see the installation FAQ on the wiki
(<https://github.com/USGS-R/loadflex/wiki/Installation-FAQ>) if you run
into trouble.

## Getting Started

To get started, load the package with `library(loadflex)` and type
`?loadflex` or `vignette('intro_to_loadflex')`.

## Development and Maintenance Status

`loadflex` is a USGS Archive Research Package: [![USGS
Status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)

Project funding has ended and our maintenance time is limited, but we do
attempt to provide bug fixes and lightweight support as we are able.
Submit questions or suggestions to
<https://github.com/USGS-R/loadflex/issues>.

## Contributing

We want to encourage a warm, welcoming, and safe environment for
contributing to this project. See
[CODE_OF_CONDUCT.md](https://github.com/USGS-R/streamMetabolizer/blob/main/CODE_OF_CONDUCT.md)
for more information.

For technical details on how to contribute, see
[CONTRIBUTING.md](https://github.com/USGS-R/streamMetabolizer/blob/main/CONTRIBUTING.md)

## Contributing

We want to encourage a warm, welcoming, and safe environment for
contributing to this project. See
[CODE_OF_CONDUCT.md](https://github.com/USGS-R/loadflex/blob/main/CODE_OF_CONDUCT.md)
for more information.

For technical details on how to contribute, see
[CONTRIBUTING.md](https://github.com/USGS-R/loadflex/blob/main/CONTRIBUTING.md)

## Development History

`loadflex` was created 2013-2015 by [Alison
Appling](https://github.com/aappling-usgs) and [Miguel
Leon](https://github.com/miguelcleon) with the support of [Bill
McDowell](https://colsa.unh.edu/faculty/mcdowell) and the [McDowell
lab](http://wrrc.unh.edu/mcdowell-lab-current) at the University of New
Hampshire. Funding for the project was provided by the National Science
Foundation, USDA National Institute of Food and Agriculture, and the NH
Agricultural Experiment Station. Funding for the example dataset from
the Lamprey River was provied by the EPA, NH Water Resources Research
Center, NH Agricultural Experiment Station, NH Sea Grant, USGS, and NSF.

Additional development and maintenance in 2016-2017 was done by [Alison
Appling](https://github.com/aappling-usgs), [Lindsay
Platt](https://github.com/lindsayplatt), and [David
Watkins](https://github.com/wdwatkins) with support from the USGS
National Water Quality Program and the USGS Office of Water Information.

## Model Archive

The following version of R and package dependencies were used most
recently to pass the embedded tests within this package. There is no
guarantee of reproducible results using future versions of R or updated
versions of package dependencies; however, we aim to test and update
future modeling environments.

<!-- Run and paste manually after edits, only when tests pass locally -->

``` r
> sessioninfo::session_info()

## (TBD)
```

## Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
