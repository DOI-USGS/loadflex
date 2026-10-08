# Correlation functions in loadflex

Correlations of residuals or prediction errors are important at several
points in the process of estimating solute concentrations or fluxes.
Directly estimating these correlations is difficult, though sometimes
possible. The functions listed here exist to help the user in (1)
estimating correlations from data, and (2) asserting correlation
structures when empirical estimates are weak or unavailable.

### 1D Correlation Functions

These are functions that produce one or more correlation coefficients
from pairs of dates.

- [`rhoEqualDates`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)

- [`rho1DayBand`](http://doi-usgs.github.io/loadflex/reference/correlations-1D.md)

### 1D Correlation Function Generators

These are functions that produce functions that produce one or more
correlation coefficients.

- [`getRhoFirstOrderFun`](http://doi-usgs.github.io/loadflex/reference/getRhoFirstOrderFun.md)

### 2D Correlation Functions

These are functions that produce a 2D correlation matrix from a vector
of dates.

- [`cormatEqualDates`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)

- [`cormat1DayBand`](http://doi-usgs.github.io/loadflex/reference/correlations-2D.md)

### 2D Correlation Function Generators

These are functions that produce functions that produce 2D correlation
matrices.

- [`getCormatCustom`](http://doi-usgs.github.io/loadflex/reference/getCormatCustom.md)

- [`getCormatTaoBand`](http://doi-usgs.github.io/loadflex/reference/getCormatTaoBand.md)

- [`getCormatFirstOrder`](http://doi-usgs.github.io/loadflex/reference/getCormatFirstOrder.md)
