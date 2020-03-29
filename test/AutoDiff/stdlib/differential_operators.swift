// RUN: %empty-directory(%t)
// RUN: %target-build-swift -enable-experimental-differentiable-programming %s -emit-sil
// RUN: %target-build-swift -enable-experimental-differentiable-programming %s -o %t/differential_operators
// RUN: %target-run %t/differential_operators
// REQUIRES: executable_test

import _Differentiation

import StdlibUnittest

var DifferentialOperatorTestSuite = TestSuite("Tuple")

DifferentialOperatorTestSuite.test("foo") {
  expectEqual(1, 1)
}

func exampleVJP(_ x: Float) -> (Float, (Float) -> Float) {
  (x * x, { 2 * $0 })
}

DifferentialOperatorTestSuite.test("differentiableFunction_callOriginal") {
  expectEqual(differentiableFunction(from: exampleVJP)(10), 100)
}

runAllTests()
