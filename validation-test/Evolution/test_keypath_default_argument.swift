// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import keypath_default_argument


var KeypathDefaultArgumentTest = TestSuite("KeypathDefaultArgument")

KeypathDefaultArgumentTest.test("ChangeStoredToComputed") {
  var t = ResilientStruct()

  expectEqual(t.change(), 10)
  expectEqual(t.change(\.value), 10)
}

runAllTests()
