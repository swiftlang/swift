// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

// Test suite for differentiable higher order functions for collections
// such as `differentiableMap(_:)` and `differentiableReduce(_:)`.
var CollectionHOFTests = TestSuite("CollectionHOF")

let xx: [Float] = [1, 2, 3, 4, 5]

CollectionHOFTests.test("differentiableMap(_:)") {
  func double(_ xx: [Float]) -> [Float] {
    xx.differentiableMap { $0 * $0 }
  }
  expectEqual([], pullback(at: xx, in: double)([]))
  expectEqual([0], pullback(at: xx, in: double)([0]))
  expectEqual([2], pullback(at: xx, in: double)([1]))
  expectEqual([2, 4, 6, 8, 10], pullback(at: xx, in: double)([1, 1, 1, 1, 1]))
}

CollectionHOFTests.test("differentiableReduce(_:)") {
  func product(_ xx: [Float]) -> Float {
    xx.differentiableReduce(1) { $0 * $1 }
  }
  expectEqual([1], gradient(at: [0], in: product))
  expectEqual([1], gradient(at: [1], in: product))
  expectEqual([120, 60, 40, 30, 24], gradient(at: xx, in: product))
}

runAllTests()
