// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift -D HAS_NONCOPYABLE_GENERICS)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-5.9 -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-6 -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-6 -O -D HAS_NONCOPYABLE_GENERICS)

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
#if HAS_NONCOPYABLE_GENERICS
  k = c.x
  expectEqual(k, 2)
  c.x = 11
  expectEqual(c.x, 11)
#endif
  k = c.mutMethod(-13)
  expectEqual(k, -13)
}

func borrowNC(_ x: borrowing NonCopyable) -> CInt {
  return x.method(3)
}

func inoutNC(_ x: inout NonCopyable, _ y: CInt) -> CInt {
  return x.mutMethod(y)
}

MoveOnlyCxxValueType.test("Test move only field access in holder") {
  var c = NonCopyableHolder(11)
  var k = borrowNC(c.x)
  expectEqual(k, 33)
  k = inoutNC(&c.x, 7)
  expectEqual(k, 7)
  expectEqual(c.x.x, 7)
  c.x.x = 78
  expectEqual(c.x.x, 78)
  c.x.mutMethod(5)
  expectEqual(c.x.x, 5)
}

#if HAS_NONCOPYABLE_GENERICS
MoveOnlyCxxValueType.test("Test move only field access in derived holder") {
  var c = NonCopyableHolderDerivedDerived(-11)
  var k = borrowNC(c.x)
  expectEqual(k, -33)
  expectEqual(c.getActualX(), -11)
  k = inoutNC(&c.x, 7)
  expectEqual(k, 7)
  expectEqual(c.x.x, 7)
  c.x.x = 78
  expectEqual(c.x.x, 78)
  c.x.mutMethod(5)
  expectEqual(c.x.x, 5)
}
#endif

runAllTests()
