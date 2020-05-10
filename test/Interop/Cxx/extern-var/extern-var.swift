// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/extern-var.cpp -I %S/Inputs -o %t/extern-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/extern-var %t/extern-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/extern-var
// RUN: %target-run %t/extern-var
//
// REQUIRES: executable_test

import ExternVar
import StdlibUnittest

var ExternVarTestSuite = TestSuite("ExternVarTestSuite")

ExternVarTestSuite.test("write-from-swift") {
  counter = 42
  expectEqual(42, counter)
  expectEqual(42, getCounterFromCxx())
}

ExternVarTestSuite.test("write-from-cxx") {
  setCounterFromCxx(84)
  expectEqual(84, counter)
  expectEqual(84, getCounterFromCxx())
}

ExternVarTestSuite.test("namespaced-write-from-swift") {
  Namespaced.counter = 42
  expectEqual(42, Namespaced.counter)
  expectEqual(42, Namespaced.getCounterFromCxx())
}

ExternVarTestSuite.test("namespaced-write-from-cxx") {
  Namespaced.setCounterFromCxx(84)
  expectEqual(84, Namespaced.counter)
  expectEqual(84, Namespaced.getCounterFromCxx())
}

// Check that variables with identical names in different namespaces don't
// collide in any intermediate representation of the compiler.
ExternVarTestSuite.test("no-collisions") {
  counter = 12
  Namespaced.counter = 42
  expectEqual(12, counter)
  expectEqual(42, Namespaced.counter)
}

runAllTests()
