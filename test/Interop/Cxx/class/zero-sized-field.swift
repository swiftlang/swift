// RUN: %empty-directory(%t/index)
// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xcc -std=c++20 -Xfrontend -index-store-path -Xfrontend %t/index)
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
  let _ : Empty.type = 6
  expectEqual(s.a, 5)
  expectEqual(s.a, s.get_a())
  expectEqual(s2.c, 7)
  expectEqual(s2.c, s2.get_c())
  expectEqual(takesZeroSizedInCpp(s2), 5)
  expectEqual(s.b.getNum(), 42)
}

FieldsTestSuite.test("Optional field padding reused") {
  var s = ReuseOptionalFieldPadding()
  let opt = s.getOptional()
  expectEqual(Int(opt.pointee), 2)
  s.c = 5
  expectEqual(Int(s.offset()),  MemoryLayout<ReuseOptionalFieldPadding>.offset(of: \.c)!)
  expectEqual(s.c, 5)
  expectEqual(s.get_c(), 5)
  s.set_c(6)
  expectEqual(s.c, 6)
  expectEqual(s.get_c(), 6)
  let s2 = s
  expectEqual(s2.c, 6)
  expectEqual(s2.get_c(), 6)
}

FieldsTestSuite.test("Typedef'd optional field padding reused") {
  var s = ReuseOptionalFieldPaddingWithTypedef()
  s.c = 5
  expectEqual(Int(s.offset()),  MemoryLayout<ReuseOptionalFieldPadding>.offset(of: \.c)!)
  expectEqual(s.c, 5)
  expectEqual(s.get_c(), 5)
  s.set_c(6)
  expectEqual(s.c, 6)
  expectEqual(s.get_c(), 6)
  let s2 = s
  expectEqual(s2.c, 6)
  expectEqual(s2.get_c(), 6)
}

FieldsTestSuite.test("Non-standard-layout field padding reused") {
  var s = ReuseNonStandardLayoutFieldPadding()
  s.c = 5
  expectEqual(Int(s.offset()),  MemoryLayout<ReuseNonStandardLayoutFieldPadding>.offset(of: \.c)!)
  expectEqual(s.c, 5)
  expectEqual(s.get_c(), 5)
  s.set_c(6)
  expectEqual(s.c, 6)
  expectEqual(s.get_c(), 6)
  let s2 = s
  expectEqual(s2.c, 6)
  expectEqual(s2.get_c(), 6)
}

FieldsTestSuite.test("Non-standard-layout field padding in templated class reused") {
  var s = ReuseDependentFieldPaddingInt()
  s.c = 5
  expectEqual(Int(s.offset()),  MemoryLayout<ReuseDependentFieldPaddingInt>.offset(of: \.c)!)
  expectEqual(s.c, 5)
  expectEqual(s.get_c(), 5)
  s.set_c(6)
  expectEqual(s.c, 6)
  expectEqual(s.get_c(), 6)
  let s2 = s
  expectEqual(s2.c, 6)
  expectEqual(s2.get_c(), 6)
}

FieldsTestSuite.test("Last field is no unique address") {
  var s = LastFieldNoUniqueAddress()

  s.c = 5
  expectEqual(s.c, 5)

  s.c = 6
  expectEqual(s.c, 6)
}

runAllTests()
