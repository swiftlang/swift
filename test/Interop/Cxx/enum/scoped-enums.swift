// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import ScopedEnums
import StdlibUnittest

var ScopedEnumsTestSuite = TestSuite("Scoped Enums")

ScopedEnumsTestSuite.test("Make and compare") {
  let val: ScopedEnumDefined = .x
  expectEqual(val, .x)
}

ScopedEnumsTestSuite.test("Make and compare (not equal)") {
  let val: ScopedEnumDefined = .x
  expectNotEqual(val, .y)
}

func makeScopedEnumBasic() -> ScopedEnumBasic { .z }

ScopedEnumsTestSuite.test("Make and compare (ScopedEnumBasic)") {
  let val: ScopedEnumBasic = .x
  expectNotEqual(val, makeScopedEnumBasic())
  expectEqual(.z, makeScopedEnumBasic())
}

ScopedEnumsTestSuite.test("Make and compare (ScopedEnumCharDefined)") {
  expectEqual(ScopedEnumCharDefined(rawValue: 2), .y)
  expectNotEqual(ScopedEnumCharDefined(rawValue: 2), ScopedEnumCharDefined(rawValue: 0))
}

ScopedEnumsTestSuite.test("Make and compare (ScopedEnumNegativeElement)") {
  expectEqual(ScopedEnumNegativeElement(rawValue: -1), .x)
  expectNotEqual(ScopedEnumNegativeElement(rawValue: 0), .x)
}

ScopedEnumsTestSuite.test("Make and compare (MiddleDefinedScopedEnum)") {
  expectEqual(MiddleDefinedScopedEnum(rawValue: 42), .y)
  expectEqual(MiddleDefinedScopedEnum(rawValue: 43), .z)
}

ScopedEnumsTestSuite.test("Make and compare (ScopedEnumChar32)") {
  let val: ScopedEnumChar32 = .x
  expectEqual(val.rawValue, 0)
  expectEqual(ScopedEnumChar32(rawValue: 42), .y)
  expectNotEqual(ScopedEnumChar32(rawValue: 0), .y)
}

ScopedEnumsTestSuite.test("Make and compare (ScopedEnumChar16)") {
  let val: ScopedEnumChar16 = .a
  expectEqual(val.rawValue, 0)
  expectEqual(ScopedEnumChar16(rawValue: 1), .b)
}

#if !os(Windows)
ScopedEnumsTestSuite.test("Make and compare (ScopedEnumWChar)") {
  let val: ScopedEnumWChar = .x
  expectEqual(val.rawValue, 0)
  expectEqual(ScopedEnumWChar(rawValue: 7), .y)
  expectNotEqual(ScopedEnumWChar(rawValue: 0), .y)
}
#endif

runAllTests()
