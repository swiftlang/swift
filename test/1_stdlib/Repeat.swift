// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift


let RepeatTests = TestSuite("Repeat")
RepeatTests.test("Attributes") {
  let r = repeatElement("repeat", count: 42)
  expectEqual(r.count, 42)
  expectEqual(r.startIndex, 0)
  expectEqual(r.endIndex, 42)
  expectEqual(r.repeatedValue, "repeat")
}

RepeatTests.test("Non-negative count") {
  expectCrashLater()
  repeatElement("repeat", count: -42)
}

runAllTests()
