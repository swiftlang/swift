// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

let RepeatTests = TestSuite("Repeat")
RepeatTests.test("Attributes") {
  let r = Repeat(count: 42, repeatedValue: "repeat")
  expectEqual(r.count, 42)
  expectEqual(r.startIndex, 0)
  expectEqual(r.endIndex, 42)
  expectEqual(r.repeatedValue, "repeat")
}

runAllTests()
