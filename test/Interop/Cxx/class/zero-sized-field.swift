// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20)
//
// REQUIRES: executable_test

import StdlibUnittest
import MemberVariables

var FieldsTestSuite = TestSuite("Generating code with zero sized fields")

func takeTypeWithZeroSizedMember(_ x: HasZeroSizedField) {}

FieldsTestSuite.test("Zero sized field") {
  var s = HasZeroSizedField()
  s.a = 5
  s.set_c(7)
  takeTypeWithZeroSizedMember(s)
  let s2 = s
  let myInt : Empty.type = 6
  expectEqual(s.a, 5)
  expectEqual(s.a, s.get_a())
  expectEqual(s2.c, 7)
  expectEqual(s2.c, s2.get_c())
  expectEqual(takesZeroSizedInCpp(s2), 5)
  expectEqual(s.b.getNum(), 42)
}

runAllTests()
