context("text")

tryCatch({source("tests/testthat/helpers.R"); source("helpers.R")}, warning=function(w) invisible())

test_that(".sentenceCase works", {
  
  expect_equal(loadflex:::.sentenceCase("hark the herald"), "Hark The Herald")
  expect_equal(loadflex:::.sentenceCase("Hark tHE heRald"), "Hark The Herald")
  expect_equal(loadflex:::.sentenceCase(c("QUICK brown Fox","LaZY doG")), c("Quick Brown Fox","Lazy Dog"))

})


test_that(".reSpace works", {
  
  expect_equal(loadflex:::.reSpace(" angles sing   GLORIA", new.space="_"), "_angles_sing___GLORIA")
  expect_equal(loadflex:::.reSpace(" angles sing   GLORIA", new.space="_", reduce.spaces=TRUE), "angles_sing_GLORIA")
  expect_equal(loadflex:::.reSpace(" angles sing   GLORIA", new.space="*|", reduce.spaces=TRUE), "angles*|sing*|GLORIA")
  expect_equal(loadflex:::.reSpace("^Shalom^aleichem^*#)", old.space="[[:punct:]|[:blank:]]"), "_Shalom_aleichem____")
  
})
