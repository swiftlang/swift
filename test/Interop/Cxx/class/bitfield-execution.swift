// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import BitFields
import StdlibUnittest

var BitFieldsTestSuite = TestSuite("BitFields")

BitFieldsTestSuite.test("round-trip through synthesized accessors") {
  var s = BitFields()
  s.a = 5
  s.b = 20
  s.c = 1_000
  expectEqual(5, s.a)
  expectEqual(20, s.b)
  expectEqual(1_000, s.c)
}

BitFieldsTestSuite.test("setter truncates to the field width") {
  var s = BitFields()
  // `a` is 3 bits wide, so only the low 3 bits are stored.
  s.a = 0xF
  expectEqual(0x7, s.a)
  // `b` is 5 bits wide.
  s.b = 0xFF
  expectEqual(0x1F, s.b)
}

BitFieldsTestSuite.test("adjacent fields are independent") {
  var s = BitFields()
  s.a = 0x7
  s.b = 0x1F
  s.c = 0
  expectEqual(0x7, s.a)
  expectEqual(0x1F, s.b)
  expectEqual(0, s.c)
}

runAllTests()
