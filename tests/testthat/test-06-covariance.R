tryCatch({source("inst/tests/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that("1D correlation functions work", {
  numdays <- 10
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  
  plot(rhoEqualDates(egdates[30], egdates))
  abline(v=30)
  expect_manual_OK("rhoEqualDates(dates[30], dates) makes sense")
  
  plot(rho1DayBand(egdates[30], egdates))
  abline(v=30)
  expect_manual_OK("rho1DayBand(dates[30], dates) makes sense")
})

test_that("1D correlation function generators work", {
  numdays <- 10
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  
  plot(getRhoFirstOrderFun(rho=0.4, time.step=as.difftime(1, units="days"))(egdates[80], egdates))
  abline(v=80)
  expect_manual_OK("getRhoFirstOrderFun(0.4, 1day)(dates[80], dates) makes sense")
})

test_that("2D correlation functions work", {
  numdays <- 10
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  
  # Test cormatEqualDates
  print(system.time(rho_mat <- cormatEqualDates(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK(paste("cormatEqualDates generates a reasonable matrix for", length(egdates), "points in a reasonable time"))
  
  # Test cormat1DayBand
  print(system.time(rho_mat <- cormat1DayBand(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK(paste("cormat1DayBand generates a reasonable matrix for", length(egdates), "points in a reasonable time"))
  
  # Test cormatDiagonal
  print(system.time(rho_mat <- cormatDiagonal(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("cormatDiagonal generates a reasonable matrix in a reasonable time")
})

test_that("2D correlation function generators work", {
  numdays <- 2
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  
  # Test getCormatCustom(rho1DayBand)
  print("getCormatCustom(rho1DayBand)() is very slow when vectorized=FALSE:")
  numdays <- 2
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  print(system.time(rho_mat <- getCormatCustom(cor1D.function=rho1DayBand, vectorized=FALSE)(egdates)))
  numdays <- 8
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  print("For comparison: running time for cormat1DayBand(dates):")
  print(system.time(rho_mat <- cormat1DayBand(egdates)))
  print("getCormatCustom(rho1DayBand)() is less slow when vectorized=TRUE:")
  print(system.time(rho_mat <- getCormatCustom(cor1D.function=rho1DayBand, vectorized=TRUE)(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("getCormatCustom(rho1DayBand)() generates a reasonable matrix in a reasonable time")
  
  # Test getCormatCustom(rhoEqualDates)
  print("getCormatCustom(rhoEqualDates)() is very slow when vectorized=FALSE:")
  numdays <- 2
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  print(system.time(rho_mat <- getCormatCustom(cor1D.function=rhoEqualDates, vectorized=FALSE)(egdates)))
  numdays <- 8
  egdates <- strptime(sprintf("2014-02-%02d %02d", rep(1:numdays, each=24), rep(0:23, numdays)), "%Y-%m-%d %H")
  print("For comparison: running time for cormatEqualDates(dates):")
  print(system.time(rho_mat <- cormatEqualDates(egdates)))
  print("getCormatCustom(rhoEqualDates)() is less slow when vectorized=TRUE:")
  print(system.time(rho_mat <- getCormatCustom(cor1D.function=rhoEqualDates, vectorized=TRUE)(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("getCormatCustom(rhoEqualDates)() generates a reasonable matrix in a reasonable time")
  
  # Test getCormatTaoBand(0.5)
  print(system.time(rho_mat <- getCormatTaoBand(max.tao=as.difftime(0.5, units="days"))(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("getCormatTaoBand(0.5) generates a reasonable matrix in a reasonable time")
  
  # Test getCormatTaoBand(3)
  print(system.time(rho_mat <- getCormatTaoBand(max.tao=as.difftime(3, units="days"))(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("getCormatTaoBand(3) generates a reasonable matrix in a reasonable time")
  
  # Test getCormatFirstOrder(0.1)
  print(system.time(rho_mat <- getCormatFirstOrder(rho=0.1, time.step=as.difftime(1, units="days"))(egdates)))
  print(image(rho_mat, lwd=0, colorkey=TRUE))
  expect_manual_OK("getCormatFirstOrder(0.1, 1day) generates a reasonable matrix in a reasonable time")
})
