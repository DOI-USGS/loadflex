#' On package load, create a private environment in the namespace.
#' 
#' Should not be run except on building the package.
#' 
#' @keywords internal
onBuild <- function() {
  pkg_env <- new.env()
  
  # persistent variable to make sure we only notify the user once per session
  # about citing rloadest
  pkg_env$rloadest_msg_given <- FALSE
    
  return(pkg_env)
}
# assign the output of onLoad to the namespace
pkg_env <- onBuild()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
                        is subject to revision. It is being provided to meet
                        the need for timely best science. The information
                        has not received final approval by the U.S. Geological
                        Survey (USGS) and is provided on the condition that
                        neither the USGS nor the U.S. Government shall be held
                        liable for any damages resulting from the authorized
                        or unauthorized use of the information.
                        
                        Funding for loadflex expires summer 2018, 
                        after which bugfixes & new features will be minimal")
}
