// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default) | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest
import ProtectedDtor

var ProtectedDtorTestSuite = TestSuite("Protected Destructors")

ProtectedDtorTestSuite.test("Using class that inherited protected destructor") {
  var d = Derived()
  d.inDerived = 123
  expectEqual(123, d.inDerived)

  // FIXME: accessing fromBase should be fine, but doesn't work because we rely on
  // importing the base class to access its members.
  // d.fromBase = 456
  // expectEqual(456, d.fromBase)

  // Make sure inherited protected destructor is called:
  // CHECK: ~InheritMe(fromBase = 111)
}

runAllTests()
