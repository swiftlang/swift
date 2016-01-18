// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

let RepeatTests = TestSuite("Repeat")
RepeatTests.test("Attributes") {
  let r = Repeat(count: 42, repeatedValue: "repeat")
  expectEqual(r.count, 42)
  expectEqual(r.startIndex, 0)
  expectEqual(r.endIndex, 42)
  expectEqual(r.repeatedValue, "repeat")
}

runAllTests()
