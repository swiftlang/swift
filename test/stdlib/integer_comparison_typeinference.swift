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

runAllTests()
