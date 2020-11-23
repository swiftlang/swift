// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorsInvokingFunctions
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("transitive-function-constructor") {
 // expectEqual(2, increment(1))
  expectEqual(42, badIncrement())
}

TemplatesTestSuite.test("inline-experiment") {
  //expectEqual(42, goodIncrement())
}

runAllTests()
