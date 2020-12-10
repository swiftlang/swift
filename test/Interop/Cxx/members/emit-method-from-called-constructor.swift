// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsMethod
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("emit-method-from-called-constructor") {
  expectEqual(42, callConstructor(41))
}

runAllTests()
