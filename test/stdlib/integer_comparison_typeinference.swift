// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let ComparisonTypeInferenceTests = TestSuite("Comparison type inference")

ComparisonTypeInferenceTests.test("Int8") {
  expectTrue( Int8.min == 1 << 7)
  expectFalse(Int8.min != 1 << 7)
  expectTrue( Int8.max >  1 << 7)
  expectTrue( Int8.max >= 1 << 7)
  expectTrue( 1 << 7 <  Int8.max)
  expectTrue( 1 << 7 <= Int8.max)
}

ComparisonTypeInferenceTests.test("Int128") {
  if #available(SwiftStdlib 6.0, *) {
    expectTrue(Int128.max != 9_999_999_999_999_999_999_999_999_999_999_999)
    expectTrue(Int128.max > 9_999_999_999_999_999_999_999_999_999_999_999)
    expectTrue(Int128.max >= 9_999_999_999_999_999_999_999_999_999_999_999)
    expectFalse(Int128.max <= 9_999_999_999_999_999_999_999_999_999_999_999)
  }
}

ComparisonTypeInferenceTests.test("UInt128") {
  if #available(SwiftStdlib 6.0, *) {
    expectTrue(UInt128.max != 9_999_999_999_999_999_999_999_999_999_999_999)
    expectTrue(UInt128.max > 9_999_999_999_999_999_999_999_999_999_999_999)
    expectTrue(UInt128.max >= 9_999_999_999_999_999_999_999_999_999_999_999)
    expectFalse(UInt128.max <= 9_999_999_999_999_999_999_999_999_999_999_999)
  }
}

runAllTests()
