// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

var ArrayDifferentiationTests = TestSuite("ArrayDifferentiation")

ArrayDifferentiationTests.test("Array.DifferentiableView+") {
  let zero1: Array<Float>.DifferentiableView = [0, 0, 0]
  let zero2: Array<Float>.DifferentiableView = .zero
  let a: Array<Float>.DifferentiableView = [1, 2, 3]
  
  expectEqual(a + a, [2, 4, 6])
  
  expectEqual(a + zero1, [1, 2, 3])
  expectEqual(zero1 + a, [1, 2, 3])
  
  expectEqual(a + zero2, [1, 2, 3])
  expectEqual(zero2 + a, [1, 2, 3])
}

ArrayDifferentiationTests.test("Array.DifferentiableView-") {
  let zero1: Array<Float>.DifferentiableView = [0, 0, 0]
  let zero2: Array<Float>.DifferentiableView = .zero
  let a: Array<Float>.DifferentiableView = [1, 2, 3]
  
  expectEqual(a - a, [0, 0, 0])
  
  expectEqual(a - zero1, [1, 2, 3])
  expectEqual(zero1 - a, [-1, -2, -3])
  
  expectEqual(a - zero2, [1, 2, 3])
  expectEqual(zero2 - a, [-1, -2, -3])
}

runAllTests()
