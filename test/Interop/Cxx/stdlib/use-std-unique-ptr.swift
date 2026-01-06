// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)
//
// REQUIRES: executable_test

// https://github.com/apple/swift/issues/70226
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import StdUniquePtr
import CxxStdlib

var StdUniquePtrTestSuite = TestSuite("StdUniquePtr")

StdUniquePtrTestSuite.test("int") {
  let u = makeInt()
  expectEqual(u.pointee, 42)
  u.pointee = -11
  expectEqual(u.pointee, -11)
}

StdUniquePtrTestSuite.test("array") {
  var u = makeArray()
  expectEqual(u[0], 1)
  expectEqual(u[1], 2)
  expectEqual(u[2], 3)
  u[0] = 10
  expectEqual(u[0], 10)
}

StdUniquePtrTestSuite.test("custom dtor") {
  expectEqual(dtorCalled, false)
  let c = {
    _ = makeDtor()
  }
  c()
  expectEqual(dtorCalled, true)
}

StdUniquePtrTestSuite.test("Test move only types behind smart pointers") {
  let sp = getNonCopyableSharedPtr()
  let up = getNonCopyableUniquePtr()
  let f1 = sp.pointee.x
  expectEqual(f1, 42)
  expectEqual(sp.pointee.method(1), 42)
  let f2 = up.pointee.x
  expectEqual(f2, 42)
  expectEqual(up.pointee.method(1), 42)
  let _ = up.pointee.x
}

runAllTests()

