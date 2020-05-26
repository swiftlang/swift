// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/static-local-var.cpp -I %S/Inputs -o %t/static-local-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-local-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticLocalVar
import StdlibUnittest

var StaticLocalVarTestSuite = TestSuite("StaticLocalVarTestSuite")

StaticLocalVarTestSuite.test("static-local-var") {
  expectEqual(counter(), 0)
  expectEqual(counter(), 1)

  // Check that the copies of the `counter()` inline function emitted by Swift
  // and Clang share the same static local variable.
  expectEqual(counterWrapper(), 2)
}

runAllTests()
