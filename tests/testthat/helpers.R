#' Suppresses only those warnings given in messages; warnings must be presented
#' in number and order expected.
library(evaluate)
gives_exact_warnings <- function(regexp) {
  function(expr) {
    res <- evaluate(substitute(expr), parent.frame(), new_device = FALSE)
    warnings <- vapply(Filter(is.warning, res), "[[", "message", 
                       FUN.VALUE = character(1))
    errors <- vapply(Filter(is.error, res), "[[", "message", 
                     FUN.VALUE = character(1))
    if(length(errors) > 0) {
      stop(errors)
    } else if (!is.null(regexp) && length(warnings) > 0) {
      matches(regexp, all = TRUE)(warnings)
    }
    else {
      expectation(length(warnings) > 0, "no warnings given")
    }
  }
}

library(ggplot2)

# Helper for manual/interactive tests that originally asked a human to inspect a
# plot or table and press ENTER to confirm.
# Such tests cannot pass/fail on their own in an automated run and would block on
# the readline() prompt, so this helper now skips them instead.
# The test code is left in place and reported as skipped (with the test id as the
# reason) so the manual checks stay visible and can be re-enabled later by
# restoring an interactive body here.
expect_manual_OK <- function(test.id, prompt = "Look at the plot.") {
  testthat::skip(paste0("manual/interactive test: ", test.id))
}
# A test for expect_manual_OK:
# test_that("expect_manual_OK makes sense as a helper function", {
#   plot(1:25, pch=1:25)
#   expect_manual_OK(1)
#   
#   print(data.frame(x=1:3,y=6:8))
#   expect_manual_OK("'data.frame looks good'", "Look at the table. ")
# })
