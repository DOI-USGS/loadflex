
#' Require an argument to match the loadflex conventions for that argument name
#' 
#' This function is modeled on \code{match.arg}, but it is designed to 
#' facilitate consistency in how argument names are used within the 
#' \code{loadflex} package.
#' 
#' The key differences between this function and \code{match.arg} are that this 
#' function (1) decides what the available options are without reference to the 
#' defaults defined in the calling function, and (2) permits atomic but 
#' non-character choices.
#' 
#' @param arg An argument, passed as a symbol. May be a missing argument to the 
#'   calling function.
#' @param choices Optional. A vector of choices, character or logical, that 
#'   should be accepted by partial matching.
#' @param several.ok logical. If TRUE, a vector of several choices in \code{arg}
#'   will be accepted and returned as such.
#' @return The argument after matching to the allowed options for that argument 
#'   name. This may differ from the original value of \code{arg} if \code{arg} 
#'   is a truncated version of one of the character options for the argument 
#'   (identified using \code{pmatch}, as in \code{match.arg}.
#'   
#' @examples
#' # match.arg.loadflex knows about some variables by their names alone
#' flux.or.conc="conc"; flux.or.conc <- loadflex:::match.arg.loadflex(flux.or.conc)
#' attach.units=TRUE; attach.units <- loadflex:::match.arg.loadflex(attach.units)
#' 
#' # you can also pass in custom choices. as always, partial matching is available
#' ci.distrib="log"; ci.distrib <- loadflex:::match.arg.loadflex(ci.distrib, c("lognormal","normal"))
match.arg.loadflex <- function(arg, choices, several.ok=FALSE) {
  # get the argument name as a character string
  argname <- deparse(substitute(arg))
  
  # Identify the valid choices for arg given its argument name. several.ok <- 
  # FALSE is the default but may be changed within the switch() as needed. If we
  # recognize this argname, then we disallow a manual specification of choices;
  # the whole point of this function is to enforce uniformity of choices for a
  # given argument name.
  uniform.choices <- NULL
  switch(
    argname,
    "flux.or.conc"={
      uniform.choices <- c("flux", "conc")
    },
    "pred.format"={
      uniform.choices <- c("flux", "conc")
    },
    "interp.format"={
      uniform.choices <- c("flux", "conc")
    },
    "interval"={
      uniform.choices <- c("none", "confidence", "prediction")
    },
    "abs.or.rel.resids"={
      uniform.choices <- c("absolute", "relative")
    },
    "attach.units"={
      uniform.choices <- c(TRUE, FALSE)
    },
    "lin.or.log"={
      uniform.choices <- c("linear", "log")
    })
  if(is.null(uniform.choices)) {
    if(missing(choices)) {
      stop("unrecognized argument name and no manually specified choices")
    } 
    # else we use the choices passed into this function, choices <- choices
  } else {
    if(!missing(choices)) {
      stop(paste("choices should not be specified for the known argument", argname))
    } else {
      choices <- uniform.choices
    }
  }
  
  # Missing argument with no defaults
  if (is.null(arg)) {
    return(choices[1L])
  }
  
  # Error checks and the special case that !several.ok & identical(arg, choices)
  if (!several.ok) {
    if (identical(arg, choices)) 
      return(arg[1L])
    if (length(arg) > 1L) 
      stop(paste0("'",argname,"' must be of length 1"))
  } else if (length(arg) == 0L) {
    stop(paste0("'",argname,"' must be of length >= 1"))
  }
  
  # Look for a match. This works for several atomic types, including logical, 
  # numeric, and character
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    stop(gettextf(
      paste0("'",argname,"' should be one of %s"), 
      paste(if(is.character(arg)) dQuote(choices) else choices, collapse = ", ")), 
      domain = NA)
  }
  i <- i[i > 0L]
  
  if (!several.ok && length(i) > 1) {
    stop(paste0("there is more than one match in 'match.arg' for '",argname,"'"))
  }
  
  choices[i]
}