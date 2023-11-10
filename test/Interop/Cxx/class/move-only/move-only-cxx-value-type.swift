// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import MoveOnlyCxxValueType
import StdlibUnittest

var MoveOnlyCxxValueType = TestSuite("Move Only Value Types")

MoveOnlyCxxValueType.test("Test move only type member access") {
  var c = NonCopyable(2)
  let k = c.method(-2)
  expectEqual(k, -4)
  expectEqual(c.method(1), 2)
}

runAllTests()

