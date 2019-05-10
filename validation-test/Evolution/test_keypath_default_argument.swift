// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import keypath_default_argument


var KeypathDefaultArgumentTest = TestSuite("KeypathDefaultArgument")

KeypathDefaultArgumentTest.test("ChangeStoredToComputed") {
  var t = ResilientStruct()

  expectEqual(t.change(), 10)
  expectEqual(t.change(\.value), 10)
}

runAllTests()
