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
