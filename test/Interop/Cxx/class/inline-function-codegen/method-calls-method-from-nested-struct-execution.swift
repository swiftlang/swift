// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import MethodCallsMethodFromNestedStruct
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("method calls method from nested struct") {
  expectEqual(42, callMethod(41))
}

runAllTests()
