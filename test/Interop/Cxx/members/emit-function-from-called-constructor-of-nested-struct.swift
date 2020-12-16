// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsFunctionFromNestedStruct
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("emit-function-from-constructor-of-nested-struct") {
  expectEqual(42, callConstructor(41))
}

runAllTests()
