# dev/dev.R — maintainer workflow, not part of the package

# Routine checks
devtools::document()      # regenerate NAMESPACE + Rd from roxygen
devtools::check()         # full R CMD check (builds vignettes too)

# README - requires that a GITLAB_PAT is set with gitcreds::gitcreds_set() and a restart of R
devtools::build_readme()  # knit README.Rmd -> README.md after editing it

# Vignette - prebuild locally because rloadest::censReg_AMLE.fit(Y, X, "lognormal") segfaults on GitHub CI
withr::with_dir("vignettes", {
  knitr::knit(
    input  = "intro_to_loadflex.Rmd.orig",
    output = "intro_to_loadflex.Rmd"
  )
})

# --- At this point, the code can be pushed; the site will be built and deployed with GitHub actions ---

# If you want to check readiness for deployment, generate a local preview of the docs site
system.time(pkgdown::build_site(lazy = TRUE)) # lazy is sometimes faster for iterating
system.time(pkgdown::build_site())

# Release-ish
devtools::build()         # build the tarball
