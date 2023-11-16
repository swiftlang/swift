// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import MoveOnlyCxxOperators
import StdlibUnittest

var MoveOnlyCxxOperators = TestSuite("Move Only Operators")

func borrowNC(_ x: borrowing NonCopyable) -> CInt {
  return x.method(3)
}

func inoutNC(_ x: inout NonCopyable, _ y: CInt) -> CInt {
  return x.mutMethod(y)
}

func consumingNC(_ x: consuming NonCopyable) {
  // do nothing.
}

MoveOnlyCxxOperators.test("NonCopyableHolderConstDeref pointee borrow") {
  let holder = NonCopyableHolderConstDeref(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
}

runAllTests()
