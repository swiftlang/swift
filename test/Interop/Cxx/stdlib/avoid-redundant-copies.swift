// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature AddressableParameters)

// REQUIRES: executable_test
// REQUIRES: swift_feature_AddressableParameters

// https://github.com/apple/swift/issues/70226
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import StdUniquePtr
import CustomSmartPtr
import CxxStdlib

var AvoidCopiesSuite = TestSuite("AvoidRedundantCopies")

AvoidCopiesSuite.test("The pointee does not copy when passed as self") {
  let up = getNonCopyableUniquePtr()
  expectEqual(up.pointee.method(1), 42)
  expectEqual(up.pointee.method(1), 42)
  let cup = getCopyCountedUniquePtr();
  expectEqual(cup.pointee.getCopies(), 0)
  cup.pointee.method()
  cup.pointee.constMethod()
  let _ = cup.pointee.field
  expectEqual(cup.pointee.getCopies(), 0)
  let copy = cup.pointee
  expectEqual(copy.getCopies(), 1)
}

AvoidCopiesSuite.test("The custom smart pointer pointee does not copy when passed as self") {
  let myptr = getPtr();
  expectEqual(myptr.pointee.getCopies(), 0)
  myptr.pointee.method()
  myptr.pointee.constMethod()
  let _ = myptr.pointee.field
  expectEqual(myptr.pointee.getCopies(), 0)
  let copy = myptr.pointee
  expectEqual(copy.getCopies(), 1)
}

runAllTests()
