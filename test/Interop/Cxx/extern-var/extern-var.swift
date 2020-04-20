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
  expectEqual(counter, 0)
  expectEqual(getCounterFromCxx(), 0)
}

ExternVarTestSuite.test("write-from-swift") {
  counter = 42
  expectEqual(counter, 42)
  expectEqual(getCounterFromCxx(), 42)
}

ExternVarTestSuite.test("write-from-cxx") {
  setCounterFromCxx(84)
  expectEqual(counter, 84)
  expectEqual(getCounterFromCxx(), 84)
}

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-read") {
//   expectEqual(Namespaced.counter, 0)
//   expectEqual(Namespaced.getCounterFromCxx(), 0)
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-write-from-swift") {
//   Namespaced.counter = 42
//   expectEqual(Namespaced.counter, 42)
//   expectEqual(Namespaced.getCounterFromCxx(), 42)
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("namespaced-write-from-cxx") {
//   Namespaced.setCounterFromCxx(84)
//   expectEqual(Namespaced.counter, 84)
//   expectEqual(Namespaced.getCounterFromCxx(), 84)
// }

//FIXME mangle non-top-level var names to prevent name collisions
// ExternVarTestSuite.test("no-collisions") {
//   counter = 12
//   Namespaced.counter = 42
//   expectEqual(counter, 12)
//   expectEqual(Namespaced.counter, 42)
// }

runAllTests()
