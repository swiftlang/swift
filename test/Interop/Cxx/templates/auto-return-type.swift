// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test

import AutoReturnType
import StdlibUnittest

var AutoTestSuite = TestSuite("AutoReturnTypeTestSuite")

AutoTestSuite.test("method from class template") {
  let x = HasMethodReturningAutoInt(t: 123)
  expectEqual(x.getT(), 123)
  expectEqual(x.getConstant(), 42.42)
}

runAllTests()
