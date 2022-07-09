// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsFunctionFromNestedCalls
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("constructor calls function from nested calls") {
  let holder = Hold42WithLongInitCallGraph()
  expectEqual(42, holder.m)
}

runAllTests()
