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

# A helper function for testing: passes if the user looks at the plot and
# presses enter, fails if the user enters anything else
expect_manual_OK <- function(test.id, prompt="Look at the plot.") {
  user.input <- readline(paste0("Test '",test.id, "': ", prompt, " Hit ENTER for OK; enter any character otherwise: "))
  user.gives.OK <- user.input == ""
  expect_true(user.gives.OK, label=paste0("Test '",test.id,"' okayed by user"))
}
# A test for expect_manual_OK:
# test_that("expect_manual_OK makes sense as a helper function", {
#   plot(1:25, pch=1:25)
#   expect_manual_OK(1)
#   
#   print(data.frame(x=1:3,y=6:8))
#   expect_manual_OK("'data.frame looks good'", "Look at the table. ")
# })
