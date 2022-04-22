// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsFunctionFromNestedStruct
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("constructor calls function from nested struct (explicit)") {
  expectEqual(42, callConstructor(41))
}

MembersTestSuite.test("constructor calls function from nested struct (implicit)") {
  let holder = HoldMemberThatHolds42()
  expectEqual(42, holder.holder.m)
}

runAllTests()
