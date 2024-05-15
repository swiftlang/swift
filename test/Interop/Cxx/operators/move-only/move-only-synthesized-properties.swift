// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift -D HAS_NONCOPYABLE_GENERICS)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-5.9 -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=swift-6 -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift -O)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift -O -D HAS_NONCOPYABLE_GENERICS)
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

MoveOnlyCxxOperators.test("testNonCopyableHolderPairedDeref pointee borrow") {
  var holder = NonCopyableHolderPairedDeref(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
  k = inoutNC(&holder.pointee, -1)
  expectEqual(k, -1)
  expectEqual(holder.pointee.x, -1)
  holder.pointee.mutMethod(3)
  expectEqual(holder.pointee.x, 3)
  holder.pointee.x = 34
  expectEqual(holder.pointee.x, 34)
#if SUPPORT_CONSUME
  consumingNC(holder.pointee)
  expectEqual(holder.pointee.x, 0)
#endif
}

MoveOnlyCxxOperators.test("testNonCopyableHolderMutDeref pointee borrow") {
  var holder = NonCopyableHolderMutDeref(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
  k = inoutNC(&holder.pointee, -1)
  expectEqual(k, -1)
  expectEqual(holder.pointee.x, -1)
  holder.pointee.mutMethod(3)
  expectEqual(holder.pointee.x, 3)
  holder.pointee.x = 34
  expectEqual(holder.pointee.x, 34)
#if SUPPORT_CONSUME
  consumingNC(holder.pointee)
  expectEqual(holder.pointee.x, 0)
#endif
}

MoveOnlyCxxOperators.test("testNonCopyableHolderValueConstDeref pointee value") {
  let holder = NonCopyableHolderValueConstDeref(11)
  var k = holder.pointee
  var k2 = holder.pointee
  expectEqual(k.x, k2.x)
}

MoveOnlyCxxOperators.test("testNonCopyableHolderValueMutDeref pointee value") {
  var holder = NonCopyableHolderValueMutDeref(11)
  var k = holder.pointee
  var k2 = holder.pointee
  expectEqual(k.x, k2.x)
}

#if HAS_NONCOPYABLE_GENERICS
MoveOnlyCxxOperators.test("NonCopyableHolderConstDerefDerivedDerived pointee borrow") {
  let holder = NonCopyableHolderConstDerefDerivedDerived(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
}

MoveOnlyCxxOperators.test("testNonCopyableHolderPairedDerefDerivedDerived pointee borrow") {
  var holder = NonCopyableHolderPairedDerefDerivedDerived(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
  k = inoutNC(&holder.pointee, -1)
  expectEqual(k, -1)
  expectEqual(holder.pointee.x, -1)
  holder.pointee.mutMethod(3)
  expectEqual(holder.pointee.x, 3)
  holder.pointee.x = 34
  expectEqual(holder.pointee.x, 34)
#if SUPPORT_CONSUME
  consumingNC(holder.pointee)
  expectEqual(holder.pointee.x, 0)
#endif
}

MoveOnlyCxxOperators.test("testNonCopyableHolderMutDerefDerivedDerived pointee borrow") {
  var holder = NonCopyableHolderMutDerefDerivedDerived(11)
  var k = borrowNC(holder.pointee)
  expectEqual(k, 33)
  k = holder.pointee.method(2)
  expectEqual(k, 22)
  k = holder.pointee.x
  expectEqual(k, 11)
  k = inoutNC(&holder.pointee, -1)
  expectEqual(k, -1)
  expectEqual(holder.pointee.x, -1)
  holder.pointee.mutMethod(3)
  expectEqual(holder.pointee.x, 3)
  holder.pointee.x = 34
  expectEqual(holder.pointee.x, 34)
#if SUPPORT_CONSUME
  consumingNC(holder.pointee)
  expectEqual(holder.pointee.x, 0)
#endif
}

MoveOnlyCxxOperators.test("testNonCopyableHolderValueConstDerefDerivedDerived pointee value") {
  let holder = NonCopyableHolderValueConstDerefDerivedDerived(11)
  var k = holder.pointee
  expectEqual(k.x, 11)
  var k2 = holder.pointee
  expectEqual(k.x, k2.x)
}

MoveOnlyCxxOperators.test("testNonCopyableHolderValueMutDerefDerivedDerived pointee value") {
  let holder = NonCopyableHolderValueMutDerefDerivedDerived(23)
  var k = holder.pointee
  expectEqual(k.x, 23)
  var k2 = holder.pointee
  expectEqual(k.x, k2.x)
}
#endif

runAllTests()
