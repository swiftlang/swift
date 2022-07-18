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

runAllTests()
