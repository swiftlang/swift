// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/enum-closed-raw-value.h)
// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -import-objc-header %S/Inputs/enum-closed-raw-value.h)

// REQUIRES: executable_test

import StdlibUnittest

var EnumClosedRawValueTestSuite = TestSuite("NS_CLOSED_ENUM raw value validation")

EnumClosedRawValueTestSuite.test("IntEnum: valid raw values") {
  let zero = IntEnum(rawValue: 0)
  expectNotNil(zero)
  expectEqual(zero!, .zero)

  let one = IntEnum(rawValue: 1)
  expectNotNil(one)
  expectEqual(one!, .one)
}

EnumClosedRawValueTestSuite.test("IntEnum: invalid raw values return nil") {
  // This should pass with the fix - invalid values return nil
  expectNil(IntEnum(rawValue: 100))
  expectNil(IntEnum(rawValue: -1))
  expectNil(IntEnum(rawValue: 2))
}

EnumClosedRawValueTestSuite.test("NonContiguousEnum: valid raw values") {
  let first = NonContiguousEnum(rawValue: 10)
  expectNotNil(first)
  expectEqual(first!, .first)

  let second = NonContiguousEnum(rawValue: 20)
  expectNotNil(second)
  expectEqual(second!, .second)

  let third = NonContiguousEnum(rawValue: 30)
  expectNotNil(third)
  expectEqual(third!, .third)
}

EnumClosedRawValueTestSuite.test("NonContiguousEnum: invalid raw values between valid ones") {
  expectNil(NonContiguousEnum(rawValue: 0))
  expectNil(NonContiguousEnum(rawValue: 15))
  expectNil(NonContiguousEnum(rawValue: 25))
  expectNil(NonContiguousEnum(rawValue: 100))
}

EnumClosedRawValueTestSuite.test("NegativeEnum: valid raw values including negatives") {
  let negative = NegativeEnum(rawValue: -1)
  expectNotNil(negative)
  expectEqual(negative!, .negative)

  let zero = NegativeEnum(rawValue: 0)
  expectNotNil(zero)
  expectEqual(zero!, .zero)

  let positive = NegativeEnum(rawValue: 1)
  expectNotNil(positive)
  expectEqual(positive!, .positive)
}

EnumClosedRawValueTestSuite.test("NegativeEnum: invalid raw values") {
  expectNil(NegativeEnum(rawValue: -2))
  expectNil(NegativeEnum(rawValue: 2))
  expectNil(NegativeEnum(rawValue: 100))
}

EnumClosedRawValueTestSuite.test("OpenEnum: accepts ANY raw value for C compatibility") {
  // Open (non-frozen) enums should accept arbitrary values
  expectNotNil(OpenEnum(rawValue: 0))
  expectNotNil(OpenEnum(rawValue: 1))

  // Invalid values should still succeed for open enums
  expectNotNil(OpenEnum(rawValue: 100))
  expectNotNil(OpenEnum(rawValue: -1))
}

EnumClosedRawValueTestSuite.test("SingleCaseEnum: only valid value") {
  let only = SingleCaseEnum(rawValue: 42)
  expectNotNil(only)
  expectEqual(only!, .only)
}

EnumClosedRawValueTestSuite.test("SingleCaseEnum: invalid raw values") {
  expectNil(SingleCaseEnum(rawValue: 0))
  expectNil(SingleCaseEnum(rawValue: 41))
  expectNil(SingleCaseEnum(rawValue: 43))
}

EnumClosedRawValueTestSuite.test("Switch exhaustivity: frozen enums don't need default") {
  // Frozen enums should be exhaustive - no default needed
  let value = IntEnum.zero
  switch value {
  case .zero:
    expectTrue(true) // Expected path
  case .one:
    expectTrue(false, "Wrong case")
  }
}

EnumClosedRawValueTestSuite.test("Switch with invalid raw value scenario from bug report") {
  // This was the crash scenario from issue #85701
  // With the fix, init should return nil, so this branch never executes
  if let ie = IntEnum(rawValue: 100) {
    switch ie {
    case .zero:
      expectTrue(false, "Should not reach here - rawValue 100 is invalid")
    case .one:
      expectTrue(false, "Should not reach here - rawValue 100 is invalid")
    }
  } else {
    // This is the expected path with the fix
    expectTrue(true, "Correctly returned nil for invalid raw value")
  }
}

runAllTests()
