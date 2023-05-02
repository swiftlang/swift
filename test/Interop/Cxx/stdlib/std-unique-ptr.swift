// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import StdUniquePtr
#if os(Linux)
import CxxStdlib
// FIXME: import CxxStdlib.string once libstdc++ is split into submodules.
#else
import CxxStdlib.memory
#endif

var StdUniquePtrTestSuite = TestSuite("StdUniquePtr")

StdUniquePtrTestSuite.test("int") {
  let u = makeInt()
  expectEqual(u.pointee, 42)
}

StdUniquePtrTestSuite.test("array") {
  var u = makeArray()
  expectEqual(u[0], 1)
// Over consume:
//  expectEqual(u[1], 2)
//  expectEqual(u[2], 3)
// Crash:
//  u[0] = 10
//  expectEqual(u[0], 10)
}

StdUniquePtrTestSuite.test("custom dtor") {
  expectEqual(dtorCalled, false)
  let c = {
    _ = makeDtor()
  }
  c()
  expectEqual(dtorCalled, true)
}

runAllTests()

