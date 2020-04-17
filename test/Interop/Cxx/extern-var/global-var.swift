// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/global-var.cc -I %S/Inputs -fPIC -o %t/global-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/global-var %t/global-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/global-var
// RUN: %target-run %t/global-var
//
// REQUIRES: executable_test

import GlobalVar
import StdlibUnittest

var StaticsTestSuite = TestSuite("global-var")

StaticsTestSuite.test("global-var") {
  expectEqual(counter, 0)
  expectEqual(count(), 1)
  expectEqual(counter, 1)
  counter = 42
  expectEqual(counter, 42)
  expectEqual(count(), 43)
}

StaticsTestSuite.test("namespaced-global-var") {
  expectEqual(Namespaced.counter, 0)
  expectEqual(Namespaced.count(), 1)
  expectEqual(Namespaced.counter, 1)
  Namespaced.counter = 42
  expectEqual(Namespaced.counter, 42)
  expectEqual(Namespaced.count(), 43)
}

runAllTests()
