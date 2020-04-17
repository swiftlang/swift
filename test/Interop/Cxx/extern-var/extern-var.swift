// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/extern-var.cc -I %S/Inputs -fPIC -o %t/extern-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/extern-var %t/extern-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/extern-var
// RUN: %target-run %t/extern-var
//
// REQUIRES: executable_test

import ExternVar
import StdlibUnittest

var StaticsTestSuite = TestSuite("extern-var")

StaticsTestSuite.test("extern-var") {
  expectEqual(counter, 0)
  expectEqual(count(), 1)
  expectEqual(counter, 1)
  counter = 42
  expectEqual(counter, 42)
  expectEqual(count(), 43)
}

//FIXME mangle non-top-level var names to prevent name collisions
// StaticsTestSuite.test("namespaced-extern-var") {
//   expectEqual(Namespaced.counter, 0)
//   expectEqual(Namespaced.count(), 1)
//   expectEqual(Namespaced.counter, 1)
//   Namespaced.counter = 42
//   expectEqual(Namespaced.counter, 42)
//   expectEqual(Namespaced.count(), 43)
// }

//FIXME mangle non-top-level var names to prevent name collisions
// StaticsTestSuite.test("non-colliding-extern-vars") {
//   counter = 12
//   Namespaced.counter = 42
//   expectEqual(counter, 12)
//   expectEqual(Namespaced.counter, 42)
// }

runAllTests()
