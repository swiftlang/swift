// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import move_method_to_extension


var MoveMethodToExtensionTest = TestSuite("MoveMethodToExtension")

MoveMethodToExtensionTest.test("MoveMethodToExtension") {
  Rain().doIt()
  Snow().doIt()

}

runAllTests()

