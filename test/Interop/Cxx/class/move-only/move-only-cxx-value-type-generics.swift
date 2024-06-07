// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=upcoming-swift -O -Xfrontend -sil-verify-none)
//
// REQUIRES: executable_test



import MoveOnlyCxxValueType
import StdlibUnittest

var MoveOnlyCxxValueType = TestSuite("Move Only Value Types + Generics")

func borrowNC(_ x: borrowing NonCopyable) -> CInt {
  return x.method(3)
}

func inoutNC(_ x: inout NonCopyable, _ y: CInt) -> CInt {
  return x.mutMethod(y)
}

func consumingNC(_ x: consuming NonCopyable) {
  // do nothing.
}

MoveOnlyCxxValueType.test("Test move only type ref return pointee borrow") {
  var holder = NonCopyableHolder(11)
  var k = borrowNC(holder.__returnNonCopyableRefUnsafe().pointee)
  expectEqual(k, 33)
  k = inoutNC(&holder.__returnMutNonCopyableRefUnsafe().pointee, 2)
  expectEqual(k, 2)
  k = borrowNC(holder.__returnMutNonCopyableRefUnsafe().pointee)
  expectEqual(k, 6)
#if ALLOW_CONSUME
  consumingNC(holder.__returnMutNonCopyableRefUnsafe().pointee)
  k = borrowNC(holder.__returnNonCopyableRefUnsafe().pointee)
  expectEqual(k, -123 * 3)
#endif
}

runAllTests()
