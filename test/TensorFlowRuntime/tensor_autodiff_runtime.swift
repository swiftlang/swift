// RUN: %target-run-dynamic-compilation-swift
//
// Note: GPE testing is disabled because GPE does not interact well with
// VJP-based AD. See SR-9638.
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

TensorADTests.testAllBackends("sum") {
  let input = Tensor<Float>(randomNormal: [2, 2])
  let sumPullbackScalar = pullback(at: input) { (a: Tensor<Float>) in a.sum() }
  let sumPullbackSqueezingAxes = pullback(at: input) { (a: Tensor<Float>) in a.sum(squeezingAxes: 0, 1) }
  let sumPullbackAlongAxes = pullback(at: input) { (a: Tensor<Float>) in a.sum(alongAxes: 0, 1) }

  let expected = Tensor<Float>(ones: [2, 2])
  expectTrue(sumPullbackScalar(Tensor(1)) == expected)
  expectTrue(sumPullbackSqueezingAxes(Tensor(1)) == expected)
  expectTrue(sumPullbackAlongAxes(Tensor(1))  == expected)
  expectTrue(sumPullbackScalar(Tensor(3)) == expected * 3)
  expectTrue(sumPullbackSqueezingAxes(Tensor(3)) == expected * 3)
  expectTrue(sumPullbackAlongAxes(Tensor(3)) == expected * 3)
}

TensorADTests.testAllBackends("mean") {
  let meanGradScalar = gradient { (a: Tensor<Float>) in a.mean() }
  let meanGradSqueezingAxes = gradient { (a: Tensor<Float>) in a.mean(squeezingAxes: 0, 1) }
  let meanGradAlongAxes = gradient { (a: Tensor<Float>) in a.mean(alongAxes: 0, 1) }

  let input = Tensor<Float>(ones: [2, 2])
  let expected = Tensor<Float>(shape: [2, 2], repeating: 0.25)
  expectTrue(meanGradScalar(input) == expected)
  expectTrue(meanGradSqueezingAxes(input) == expected)
  expectTrue(meanGradAlongAxes(input) == expected)
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
