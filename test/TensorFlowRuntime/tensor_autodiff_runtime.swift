// RUN: %target-run-eager-swift
//
// Note: GPE testing is disabled because GPE does not interact well with
// VJP-based AD. See SR-9638.
//
// REQUIRES: executable_test
//
// Tensor AD runtime tests.

import TensorFlow
import StdlibUnittest
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif

var TensorADTests = TestSuite("TensorAD")

TensorADTests.testAllBackends("TestSimpleGrad") {
  func square(_ x: Tensor<Float>) -> Tensor<Float> {
    return x * x
  }
  expectEqual([0.2, 0.4, 0.6], gradient(at: [0.1, 0.2, 0.3], in: square))
  expectEqual([[20], [40]], gradient(at: [[10], [20]], in: square))
}

TensorADTests.testAllBackends("TestGenericGrad") {
  func square<T : TensorFlowFloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
    return x * x
  }
  expectEqual([0.2, 0.4, 0.6], gradient(at: Tensor([0.1, 0.2, 0.3]), in: square))
}

TensorADTests.testAllBackends("TestScalarGenericGrad") {
  // Tests TF-287.
  func negate<T : TensorFlowFloatingPoint>(_ x: Tensor<T>) -> Tensor<T> {
    return 1 - x
  }
  expectEqual(Tensor(-1), gradient(at: Tensor([0.1, 0.2, 0.3]), in: negate))
}

TensorADTests.testAllBackends("TestScalarized") {
  let grad = gradient(at: Tensor<Float>([3.0, 4.0])) { x in
    logSoftmax(x).mean().scalarized()
  }
  expectEqual(Tensor([0.23105857, -0.2310586]), grad)
}

TensorADTests.testAllBackends("+") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a + b }
  expectEqual((Tensor(1), Tensor(1)), gradient(at: Tensor(0), Tensor(0), in: f))
  expectEqual(([1], [1]), pullback(at: [1], [10], in: f)([1]))
}

TensorADTests.testAllBackends("-") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a - b }
  expectEqual((Tensor(1), Tensor(-1)), gradient(at: Tensor(0), Tensor(0), in: f))
  expectEqual(([1], [-1]), pullback(at: [1], [10], in: f)([1]))
}

TensorADTests.testAllBackends("*") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a * b }
  expectEqual(([0], [0]), gradient(at: [0], [0], in: f))
  expectEqual(([10], [1]), gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("/") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a / b }
  expectEqual(([0.1], [-0.01]), gradient(at: [1], [10], in: f))
}

TensorADTests.testAllBackends("matmul") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in matmul(a, b) }
  let v = Tensor<Float>(ones: [1, 1])
  expectEqual(([[0]], [[0]]), pullback(at: [[0]], [[0]], in: f)(v))
  expectEqual(([[10]], [[1]]), pullback(at: [[1]], [[10]], in: f)(v))
}

TensorADTests.testAllBackends("•") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a • b }
  let v = Tensor<Float>(ones: [1, 1])
  expectEqual(([[0]], [[0]]), pullback(at: [[0]], [[0]], in: f)(v))
  expectEqual(([[10]], [[1]]), pullback(at: [[1]], [[10]], in: f)(v))
}

TensorADTests.testAllBackends("negate") {
  let f = { (a: Tensor<Float>) in -a }
  expectEqual([-1], gradient(at: [0], in: f))
  expectEqual([-1], gradient(at: [10], in: f))
}

TensorADTests.testAllBackends("Abs") {
  let f = { (a: Tensor<Float>) in abs(a) }
  expectEqual([1, -1, 0], gradient(at: [3.0, -3.0, 0], in: f))
}

TensorADTests.testAllBackends("sum") {
  let input = Tensor<Float>(repeating: 42, shape: [2, 2])
  let sumPullbackScalar = pullback(at: input) { (a: Tensor<Float>) in a.sum() }
  let sumPullbackAlongAxes = pullback(at: input) { (a: Tensor<Float>) in a.sum(alongAxes: 0, 1) }

  let expected = Tensor<Float>(ones: [2, 2])
  expectEqual(expected, sumPullbackScalar(Tensor(1)))
  // expectEqual(expected, sumPullbackSqueezingAxes(Tensor(1)))
  expectEqual(expected, sumPullbackAlongAxes(Tensor(1)))
  expectEqual(expected * 3, sumPullbackScalar(Tensor(3)))
  // expectEqual(expected * 3, sumPullbackSqueezingAxes(Tensor(3)))
  expectEqual(expected * 3, sumPullbackAlongAxes(Tensor(3)))
}

TensorADTests.testAllBackends("mean") {
  let meanGradScalar = gradient { (a: Tensor<Float>) in a.mean() }
  // let meanGradSqueezingAxes = gradient { (a: Tensor<Float>) in a.mean(squeezingAxes: 0, 1) }
  let meanGradAlongAxes = gradient { (a: Tensor<Float>) in a.mean(alongAxes: 0, 1) }

  let input = Tensor<Float>(ones: [2, 2])
  let expected = Tensor<Float>(repeating: 0.25, shape: [2, 2])
  expectEqual(expected, meanGradScalar(input))
  // expectEqual(expected, meanGradSqueezingAxes(input))
  expectEqual(expected, meanGradAlongAxes(input))
}

TensorADTests.testAllBackends("expandingShape") {
  let f1 = { (a: Tensor<Float>) in a.expandingShape(at: 0).squared() }
  let f2 = { (a: Tensor<Float>) in a.squared().expandingShape(at: 0) }
  expectEqual([6, 10], pullback(at: [3, 5], in: f1)([[1, 1]]))
  expectEqual([6, 10], pullback(at: [3, 5], in: f2)([[1, 1]]))
}

TensorADTests.testAllBackends("squeezingShape") {
  let f1 = { (a: Tensor<Float>) in a.squeezingShape(at: 0).squared() }
  let f2 = { (a: Tensor<Float>) in a.squared().squeezingShape(at: 0) }
  expectEqual([[6, 10]], pullback(at: [[3, 5]], in: f1)([1, 1]))
  expectEqual([[6, 10]], pullback(at: [[3, 5]], in: f2)([1, 1]))
}

TensorADTests.testAllBackends("reshapedBackprop") {
  let f1 = { (a: Tensor<Float>) in a.reshaped(toShape: Tensor<Int32>([2, 1])).squared() }
  let f2 = { (a: Tensor<Float>) in a.squared().reshaped(toShape: Tensor<Int32>([2, 1])) }
  expectEqual([[6, 10]], pullback(at: [[3, 5]], in: f1)([[1], [1]]))
  expectEqual([[6, 10]], pullback(at: [[3, 5]], in: f2)([[1], [1]]))
}

TensorADTests.testAllBackends("reshaped") {
  let shapeTensor = Tensor<Int32>([2, 2, 2])
  let input = Tensor<Float>(ones: [2, 4])
  let reshapedPullback = pullback(at: input) { (a: Tensor<Float>) in a.reshaped(toShape: shapeTensor) }
  let reshaped = Tensor<Float>(ones: [2, 2, 2])
  expectEqual(input, reshapedPullback(reshaped))
}

TensorADTests.testAllBackends("transposed") {
  let input = Tensor<Float>(ones: [2, 3])
  let transposed = Tensor<Float>(ones: [3, 2])
  let transposedPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed() }
  let transposedPermutationsPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed(withPermutations: [1, 0]) }
  let transposedVariadicsPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed(withPermutations: 1, 0) }

  expectEqual(input, transposedPullback(transposed))
  expectEqual(input, transposedPermutationsPullback(transposed))
  expectEqual(input, transposedVariadicsPullback(transposed))
}

TensorADTests.testAllBackends("relu") {
  let f = { (a: Tensor<Float>) in relu(a) }
  expectEqual([1, 0, 0], gradient(at: [5, -5, 0], in: f))
}

TensorADTests.testAllBackends("softmax") {
  let pb = pullback(at: Tensor(ones: [2, 2])) { (a: Tensor<Float>) in softmax(a) }
  expectEqual([[0, 0], [0, 0]], pb([[1, 1], [1, 1]]))
  expectEqual([[-0.25, 0.25], [0.75, -0.75]], pb([[1, 2], [4, 1]]))
}

TensorADTests.testAllBackends("log_softmax") {
  let pb = pullback(at: Tensor(ones: [3, 3])) { (a: Tensor<Float>) in logSoftmax(a) }
  expectEqual(Tensor(repeating: 5.9604645e-08, shape: [3, 3]), pb(Tensor(ones: [3, 3])))
}

TensorADTests.testAllBackends("SR-9345: OwnedCheckpoints") {
  @differentiable(vjp: vjpFoo)
  func foo(_ x: Tensor<Float>) -> Tensor<Float> {
      return Raw.identity(x)
  }
  func vjpFoo(_ x: Tensor<Float>) -> (Tensor<Float>, (Tensor<Float>) -> Tensor<Float>) {
    return (foo(x), { v in v })
  }
  func body(_ x: Tensor<Float>) -> Tensor<Float> {
    return foo(foo(x))
  }
  let pb = pullback(at: Tensor(Float(10)), in: body)
  expectEqual(Tensor(1), pb(Tensor(1)))
}

TensorADTests.testAllBackends("SR-9804: AD refcounting") {
  func f(_ x: Tensor<Float>) -> Tensor<Float> {
    return x
  }
  expectEqual(Tensor(1), gradient(at: Tensor(0), in: f))
}

let cube: (Tensor<Float>) -> Tensor<Float> = { $0 * $0 * $0 }
TensorADTests.testAllBackends("Differentiate global") {
  expectEqual(Tensor(48), gradient(at: Tensor(4), in: cube))
}

TensorADTests.testAllBackends("Side effects") {
  let foo: @differentiable (Tensor<Float>) -> Tensor<Float> = { x in
    var a = x
    a = a + x
    a = a + x
    return a + x
  }
  expectEqual(Tensor([8, 8]), pullback(at: Tensor(4), in: foo)([1, 1]))

  func bar(x: Tensor<Float>) -> Tensor<Float> {
    var a = x
    a = a * x
    a = a * x
    return a
  }
  expectEqual(Tensor(48), gradient(at: Tensor(4), in: bar))
}

runAllTests()
