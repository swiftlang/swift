// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import move_method_to_extension


var MoveMethodToExtensionTest = TestSuite("MoveMethodToExtension")

MoveMethodToExtensionTest.test("MoveMethodToExtension") {
  Rain().doIt()
  Snow().doIt()

}

runAllTests()

