// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import MoveOnlyCxxValueType
import StdlibUnittest

var MoveOnlyCxxValueType = TestSuite("Move Only Value Types")

MoveOnlyCxxValueType.test("Test move only type member access") {
  var c = NonCopyable(2)
  var k = c.method(-2)
  expectEqual(k, -4)
  expectEqual(c.method(1), 2)
  k = c.x
  expectEqual(k, 2)
  c.x = -3
  expectEqual(c.x, -3)
  k = c.mutMethod(72)
  expectEqual(k, 72)
}

MoveOnlyCxxValueType.test("Test derived move only type member access") {
  var c = NonCopyableDerivedDerived(2)
  var k = c.method(-3)
  expectEqual(k, -6)
  expectEqual(c.method(1), 2)
  k = c.x
  expectEqual(k, 2)
  c.x = 11
  expectEqual(c.x, 11)
  k = c.mutMethod(-13)
  expectEqual(k, -13)
}

runAllTests()
