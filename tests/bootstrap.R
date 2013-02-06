library(RUnit)

## Run all the tests
testsuite <- defineTestSuite("PCICt", dirs="./", testFileRegexp = "^test_functions.R$", testFuncRegexp = "^PCICt.test.+")
PCICt.test.result <- runTestSuite(testsuite, useOwnErrorHandler=F)
printTextProtocol(PCICt.test.result)
