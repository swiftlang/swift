// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test

// TODO: See why -validate-tbd-against-ir=none is needed here

import StaticVarInitCallsFunction
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("emit-function-from-static-var-init") {
  expectEqual(42, initializeStaticVar())
}

runAllTests()
