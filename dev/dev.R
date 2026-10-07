# dev/dev.R — maintainer workflow, not part of the package

# Routine checks
devtools::document()      # regenerate NAMESPACE + Rd from roxygen
devtools::check()         # full R CMD check (builds vignettes too)

# README
devtools::build_readme()  # knit README.Rmd -> README.md after editing it

# Vignette - prebuild locally because rloadest::censReg_AMLE.fit(Y, X, "lognormal") segfaults on GitHub CI
knitr::knit(
  input  = "vignettes/intro_to_loadflex.Rmd.orig",
  output = "vignettes/intro_to_loadflex.Rmd"
)

# Docs site (local preview)
pkgdown::build_site()     # or build_site(lazy = TRUE) while iterating

# Release-ish
devtools::build()         # build the tarball
