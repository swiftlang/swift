// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

let AlgorithmTests = TestSuite("Algorithm")

AlgorithmTests.test("Min") {
  expectEqual(0, min(0, 1))
  expectEqual(0, min(1, 0))

  expectEqual(0, min(0, 1, 1))
  expectEqual(0, min(1, 0, 1))
  expectEqual(0, min(1, 1, 0))
}

AlgorithmTests.test("Max") {
  expectEqual(1, max(1, 0))
  expectEqual(1, max(0, 1))

  expectEqual(1, max(1, 0, 0))
  expectEqual(1, max(0, 1, 0))
  expectEqual(1, max(0, 0, 1))
}

runAllTests()
