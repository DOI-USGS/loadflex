#just trigger travis.
library(testthat)
library(loadflex)

test_check('loadflex', filter='01')
test_check('loadflex', filter='02')
# test_check('loadflex', filter='03') # breaks
# test_check('loadflex', filter='04') # takes forever & has interaction
# test_check('loadflex', filter='05') # segfault on travis (not appveyor or local)
# test_check('loadflex', filter='06') # just a shell of a test now
test_check('loadflex', filter='11')
test_check('loadflex', filter='21')
test_check('loadflex', filter='22')
test_check('loadflex', filter='23')
# test_check('loadflex', filter='24') # takes forever
# test_check('loadflex', filter='25') # breaks
test_check('loadflex', filter='31') # loadReg() call is commented out
# test_check('loadflex', filter='32') # loadReg() call is integral to test, probably causes segfault on travis
# test_check('loadflex', filter='33') # loadReg() calls are integral to test, probably cause segfault on travis
test_check('loadflex', filter='41')
# test_check('loadflex', filter='42') # empty test
test_check('loadflex', filter='44')
test_check('loadflex', filter='51')

# test_check('loadflex', filter='01|02|05|06|11|21|22|23|31|32|33|41|42')
