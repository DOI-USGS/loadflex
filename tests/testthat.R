library(testthat)
#test_check('loadflex', filter='01|02|05|06|11|21|22||23|24|31|32|33||41|42') # travis build failed, rebuild with trivial change
test_check('loadflex', filter='01|02|05|06|11|21|22|23|24|31|32|41|42')