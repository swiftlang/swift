// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import ConstructorCallsFunction
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("constructor calls function (explicit)") {
  expectEqual(42, callConstructor(41))
}

MembersTestSuite.test("constructor calls function (implicit)") {
  let holder = Hold42()
  expectEqual(42, holder.m)
}

MembersTestSuite.test("constructor calls template function (implicit)") {
  let holder = Hold23()
  expectEqual(23, holder.m)
}

runAllTests()
