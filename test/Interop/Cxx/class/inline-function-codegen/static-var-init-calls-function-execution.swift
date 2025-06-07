// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StaticVarInitCallsFunction
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("static var init calls function") {
  expectEqual(42, initializeStaticVar())
}

runAllTests()
