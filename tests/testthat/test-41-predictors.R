data(app2.est)
data(eg_metadata)
md <- updateMetadata(eg_metadata, flow="FLOW", dates="DATES")

#method hysep is commented out mcl - 1-22-16
# test_that("getPred_baseflow with method='hysep' works", {  
#   
#   app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, da=200000, select="sliding")
#   print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="blue") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="black") + ggplot2::theme_bw())
#   expect_manual_OK("hysep|sliding: Baseflow (black) is reasonably separated from total flow (blue)")
#   
#   app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, da=200000, select="local minimum")
#   print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="blue") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="black") + ggplot2::theme_bw())
#   expect_manual_OK("hysep|local minimum: Baseflow (black) is reasonably separated from total flow (blue)")
#   
#   app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, da=200000, select="fixed")
#   print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="blue") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="black") + ggplot2::theme_bw())
#   expect_manual_OK("hysep|fixed: Baseflow (black) is reasonably separated from total flow (blue)")
#   
# })

test_that("getPred_baseflow with method='1' works", {  
  
  app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, method="1p digital filter", alpha=0.98)
  #print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="blue") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="black") + ggplot2::theme_bw())
  expect_manual_OK("1pDF: Baseflow (black) is reasonably separated from total flow (blue)")
  
})

test_that("getPred_baseflow with method='2' works", {  
  
  app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, method="2p digital filter", alpha=0.98, BFImax=0.5)
  #print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="blue") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="black") + ggplot2::theme_bw())
  expect_manual_OK("2pDF: Baseflow (black) is reasonably separated from total flow (blue)")
  
})

test_that("getPred_isBaseflow works", {  
  
  app2.est$Baseflow <- getPred_baseflow(data=app2.est, metadata=md, method="2p digital filter", alpha=0.98, BFImax=0.5)
  app2.est$IsBaseflow <- getPred_isBaseflow(data=app2.est, metadata=md, baseflow=app2.est$Baseflow, threshold=0.8)
  #print(ggplot2::ggplot(app2.est, ggplot2::aes(x=DATES)) + ggplot2::geom_line(ggplot2::aes(y=FLOW), color="lightgrey") + ggplot2::geom_line(ggplot2::aes(y=Baseflow), color="grey") + 
  #        ggplot2::geom_point(ggplot2::aes(y=FLOW, color=IsBaseflow), size=0.3) + ggplot2::theme_bw())
  expect_manual_OK("isBaseflow: Runoff (red) is reasonably separated from baseflow (blue)")
  
})

