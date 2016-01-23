library(testthat)
library(loadflex)

test_check('loadflex', filter='01|02|11|21|31|32|42')
# 03 breaks
# 04 takes forever & has interaction
# 05 has interaction
# 06 has interaction
# 22 has interaction
# 23 has interaction
# 24 has interaction
# 25 breaks
# 33 has interaction
# 41 has interaction
# 44 is empty
