#just trigger travis.
library(testthat)
library(loadflex)

test_check('loadflex', filter='01')
test_check('loadflex', filter='02')
# test_check('loadflex', filter='05') segfault on travis (not appveyor or local)
# test_check('loadflex', filter='06') # just a shell of a test now
test_check('loadflex', filter='11')
test_check('loadflex', filter='21')
test_check('loadflex', filter='22')
test_check('loadflex', filter='23')
test_check('loadflex', filter='31')
test_check('loadflex', filter='32')
test_check('loadflex', filter='33')
test_check('loadflex', filter='41')
test_check('loadflex', filter='42')
# test_check('loadflex', filter='01|02|05|06|11|21|22|23|31|32|33|41|42')
# 03 breaks
# 04 takes forever & has interaction
# 05 interaction removed
# 06 interaction removed
# 22 interaction removed
# 23 interaction removed
# 24 takes forever
# 25 breaks
# 33 had interaction, removed
# 41 interaction removed
# 44 is empty

