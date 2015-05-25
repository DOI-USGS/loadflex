context("match.arg.loadflex")

tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("loadflex:::match.arg.loadflex knows the defaults for many args", {

  # Passes when given acceptable answers
  expect_true((function(flux.or.conc){loadflex:::match.arg.loadflex(flux.or.conc); return(TRUE)})("flux"))
  expect_true((function(pred.format){loadflex:::match.arg.loadflex(pred.format); return(TRUE)})("flux"))
  expect_true((function(interp.format){loadflex:::match.arg.loadflex(interp.format); return(TRUE)})("flux"))
  expect_true((function(interval){loadflex:::match.arg.loadflex(interval); return(TRUE)})("confidence"))
  expect_true((function(abs.or.rel.resids){loadflex:::match.arg.loadflex(abs.or.rel.resids); return(TRUE)})("relative"))
  expect_true((function(attach.units){loadflex:::match.arg.loadflex(attach.units); return(TRUE)})(FALSE))

  # Throws error when arg isn't one of the options
  expect_error((function(flux.or.conc){loadflex:::match.arg.loadflex(flux.or.conc); return(TRUE)})("wronganswer"), "should be one of")
  expect_error((function(pred.format){loadflex:::match.arg.loadflex(pred.format); return(TRUE)})("wronganswer"), "should be one of")
  expect_error((function(interp.format){loadflex:::match.arg.loadflex(interp.format); return(TRUE)})("wronganswer"), "should be one of")
  expect_error((function(interval){loadflex:::match.arg.loadflex(interval); return(TRUE)})("wronganswer"), "should be one of")
  expect_error((function(abs.or.rel.resids){loadflex:::match.arg.loadflex(abs.or.rel.resids); return(TRUE)})("wronganswer"), "should be one of")
  expect_error((function(attach.units){loadflex:::match.arg.loadflex(attach.units); return(TRUE)})("wronganswer"), "should be one of")
  
})

test_that("loadflex:::match.arg.loadflex can handle manually specified args", {
  
  expect_true((function(any.arg){loadflex:::match.arg.loadflex(any.arg, c(1,4,NA)); return(TRUE)})(1))
  expect_error((function(any.arg){loadflex:::match.arg.loadflex(any.arg, c(1,4,NA)); return(TRUE)})(0), "should be one of")
  
})

test_that("loadflex:::match.arg.loadflex can handle multiple args", {
  
  # Should reject 2 answers when several.ok==FALSE, accept them when several.ok==TRUE, reject answers that aren't in the choices
  expect_error((function(any.arg){loadflex:::match.arg.loadflex(any.arg, c(1,4,NA), several.ok=FALSE)})(c(NA,1)))
  expect_equal((function(any.arg){loadflex:::match.arg.loadflex(any.arg, c(1,4,NA), several.ok=TRUE)})(c(NA,1)), c(NA,1))
  
  # If one but not all of the values supplied are in the choices, those that 
  # overlap are returned without error. Not sure this is optimal, but it's
  # consistent with match.arg
  expect_false(length((function(any.arg){loadflex:::match.arg.loadflex(any.arg, c(1,4,NA), several.ok=TRUE)})(c(17,NA))) == 2)
  expect_false(length((function(any.arg){match.arg(any.arg, c("1","4","NA"), several.ok=TRUE)})(c("17","4"))) == 2)
  
})