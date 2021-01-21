// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsFunction
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("constructor calls function") {
  expectEqual(42, callConstructor(41))
}

runAllTests()
