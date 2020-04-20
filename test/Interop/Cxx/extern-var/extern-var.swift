// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/extern-var.cc -I %S/Inputs -fPIC -o %t/extern-var.o
// RUN: %target-build-swift %s -I %S/Inputs -o %t/extern-var %t/extern-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/extern-var
// RUN: %target-run %t/extern-var
//
// REQUIRES: executable_test

import ExternVar
import StdlibUnittest

var ExternVarTestSuite = TestSuite("ExternVarTestSuite")

ExternVarTestSuite.test("read") {
  expectEqual(0, counter)
  expectEqual(0, getCounterFromCxx())
}

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

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-read") {
//   expectEqual(0, Namespaced.counter)
//   expectEqual(0, Namespaced.getCounterFromCxx())
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-write-from-swift") {
//   Namespaced.counter = 42
//   expectEqual(42, Namespaced.counter)
//   expectEqual(42, amespaced.getCounterFromCxx())
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-write-from-cxx") {
//   Namespaced.setCounterFromCxx(84)
//   expectEqual(84, Namespaced.counter)
//   expectEqual(84, Namespaced.getCounterFromCxx())
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("no-collisions") {
//   counter = 12
//   Namespaced.counter = 42
//   expectEqual(12, counter)
//   expectEqual(42, Namespaced.counter)
// }

runAllTests()
