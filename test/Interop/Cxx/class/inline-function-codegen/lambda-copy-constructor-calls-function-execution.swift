// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import LambdaCopyConstructorCallsFunction
import StdlibUnittest

var LambdaCopyTestSuite = TestSuite("LambdaCopyTestSuite")

LambdaCopyTestSuite.test("lambda copy constructor calls function") {
  expectEqual(1, IntLambdaCopier().copyLambda())
}

runAllTests()
