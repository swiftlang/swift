// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-simple-no-vjp-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift
//
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Tensor AD runtime tests.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorAD")

TensorADTests.testAllBackends("TestSimpleGrad") {
  func square(_ x: Tensor<Float>) -> Tensor<Float> {
    return x * x
  }
  expectTrue(gradient(at: [0.1, 0.2, 0.3], in: square) == [0.2, 0.4, 0.6])
  expectTrue(gradient(at: [[10], [20]], in: square) == [[20], [40]])
}

TensorADTests.testAllBackends("+") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a + b }
  expectTrue(([1], [1]) == gradient(at: [0], [0], in: f))
  expectTrue(([1], [1]) == gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("-") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a - b }
  expectTrue(([1], [-1]) == gradient(at: [0], [0], in: f))
  expectTrue(([1], [-1]) == gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("*") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a * b }
  expectTrue(([0], [0]) == gradient(at: [0], [0], in: f))
  expectTrue(([10], [1]) == gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("/") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a / b }
  expectTrue(([0.1], [-0.01]) == gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("matmul") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in matmul(a, b) }
  expectTrue(([[0]], [[0]]) == gradient(at: [[0]], [[0]], in: f))
  expectTrue(([[10]], [[1]]) == gradient(at: [[1]], [[10]], in: f))
}

TensorADTests.testAllBackends("•") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a • b }
  expectTrue(([[0]], [[0]]) == gradient(at: [[0]], [[0]], in: f))
  expectTrue(([[10]], [[1]]) == gradient(at: [[1]], [[10]], in: f))
}

TensorADTests.testAllBackends("negate") {
  let f = { (a: Tensor<Float>) in -a }
  expectTrue([-1] == gradient(at: [0], in: f))
  expectTrue([-1] == gradient(at: [10], in: f))
}

TensorADTests.testAllBackends("SR-9345: OwnedCheckpoints") {
  @differentiable(adjoint: adjointFoo)
  func foo(_ x: Tensor<Float>) -> Tensor<Float> {
      return Raw.identity(x)
  }
  func adjointFoo(_ seed: Tensor<Float>, _ originalValue: Tensor<Float>,
                  _ x: Tensor<Float>) -> Tensor<Float> {
    return seed
  }
  func body(_ x: Tensor<Float>) -> Tensor<Float> {
    return foo(foo(x))
  }
  let pb = pullback(at: Tensor(Float(10)), in: body)
  expectEqual(Tensor(1.0), pb(Tensor(1)))
}

runAllTests()
