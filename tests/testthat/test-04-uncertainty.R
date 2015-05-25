tryCatch({source("inst/tests/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("it's easy to get log-to-linear transformations of distribution moments", {
  # meanlin, sdlin, meanlog, and sdlog args work, and the outputs are a
  # reasonable and informatively named data.frame
  expect_equal(linToLog(meanlin=1, sdlin=0.5), data.frame(meanlog=-0.1115718, sdlog=0.472380), tol=0.00001)
  expect_equal(logToLin(meanlog=1, sdlog=0.5), data.frame(meanlin= 3.0802170, sdlin=1.641572), tol=0.00001)
  
  # meanlin, sdlin, meanlog, and sdlog args work, and you can go round trip from
  # log to lin to log or from lin to log to lin
  expect_equivalent(logToLin(ms=linToLog(meanlin=1, sdlin=0.5)), list(1,0.5))
  expect_equivalent(linToLog(ms=logToLin(meanlog=1, sdlog=0.5)), list(1,0.5))
  
  # mslist args work, and you can go round trip from log to lin to log or from
  # lin to log to lin
  expect_equivalent(logToLin(ms=linToLog(ms=logToLin(ms=linToLog(ms=list(1,1))))), list(1,1))
  expect_equivalent(linToLog(ms=logToLin(ms=linToLog(ms=logToLin(ms=list(0,1))))), list(0,1))
  
  # you may supply more than one sd
  expect_equal(logToLin(ms=linToLog(meanlin=9, sdlin=c(1,3,5))), data.frame(meanlin=9, sdlin=c(1,3,5)))
  expect_equal(linToLog(ms=logToLin(meanlog=9, sdlog=c(1,3,5))), data.frame(meanlog=9, sdlog=c(1,3,5)))
})
