// RUN: %empty-directory(%t)
// RUN: %target-clangxx -c %S/Inputs/reference.cpp -I %S/Inputs -o %t/reference.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/reference %t/reference.o -Xfrontend -enable-experimental-cxx-interop
// RUN: %target-codesign %t/reference
// RUN: %target-run %t/reference
//
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Closures
import StdlibUnittest

var ClosuresTestSuite = TestSuite("Closures")

ClosuresTestSuite.test("const reference closure") {
  var x: CInt = 0
  invokeWith42ConstRef { x = $0 }
  expectEqual(42, x)
}

ClosuresTestSuite.test("mutable reference closure") {
  var x: CInt = 0
  invokeWith42Ref { x = $0.pointee }
  expectEqual(42, x)
}

runAllTests()
