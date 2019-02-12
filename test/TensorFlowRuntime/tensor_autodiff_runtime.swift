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
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorAD")

TensorADTests.testAllBackends("TestSimpleGrad") {
  func square(_ x: Tensor<Float>) -> Tensor<Float> {
    return x * x
  }
  expectTrue(gradient(at: [0.1, 0.2, 0.3], in: square) == [0.2, 0.4, 0.6])
  expectTrue(gradient(at: [[10], [20]], in: square) == [[20], [40]])
}

TensorADTests.testAllBackends("TestGenericGrad") {
  func square<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Tensor<T> {
    return x * x
  }
  expectEqual([0.2, 0.4, 0.6], gradient(at: Tensor([0.1, 0.2, 0.3]), in: square))
}

TensorADTests.testAllBackends("+") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a + b }
  expectTrue((Tensor(1), Tensor(1)) == gradient(at: Tensor(0), Tensor(0), in: f))
  expectTrue(([1], [1]) == pullback(at: [1], [10], in: f)([1]))
}

TensorADTests.testAllBackends("-") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a - b }
  expectTrue((Tensor(1), Tensor(-1)) == gradient(at: Tensor(0), Tensor(0), in: f))
  expectTrue(([1], [-1]) == pullback(at: [1], [10], in: f)([1]))
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
  let v = Tensor<Float>(ones: [1, 1])
  expectTrue(([[0]], [[0]]) == pullback(at: [[0]], [[0]], in: f)(v))
  expectTrue(([[10]], [[1]]) == pullback(at: [[1]], [[10]], in: f)(v))
}

TensorADTests.testAllBackends("•") {
  let f = { (a: Tensor<Float>, b: Tensor<Float>) in a • b }
  let v = Tensor<Float>(ones: [1, 1])
  expectTrue(([[0]], [[0]]) == pullback(at: [[0]], [[0]], in: f)(v))
  expectTrue(([[10]], [[1]]) == pullback(at: [[1]], [[10]], in: f)(v))
}

TensorADTests.testAllBackends("negate") {
  let f = { (a: Tensor<Float>) in -a }
  expectTrue([-1] == gradient(at: [0], in: f))
  expectTrue([-1] == gradient(at: [10], in: f))
}

TensorADTests.testAllBackends("sum") {
  let input = Tensor<Float>(randomNormal: [2, 2])
  let sumPullbackScalar = pullback(at: input) { (a: Tensor<Float>) in a.sum() }
  // let sumPullbackSqueezingAxes = pullback(at: input) { (a: Tensor<Float>) in a.sum(squeezingAxes: 0, 1) }
  let sumPullbackAlongAxes = pullback(at: input) { (a: Tensor<Float>) in a.sum(alongAxes: 0, 1) }

  let expected = Tensor<Float>(ones: [2, 2])
  expectTrue(sumPullbackScalar(Tensor(1)) == expected)
  // expectTrue(sumPullbackSqueezingAxes(Tensor(1)) == expected)
  expectTrue(sumPullbackAlongAxes(Tensor(1))  == expected)
  expectTrue(sumPullbackScalar(Tensor(3)) == expected * 3)
  // expectTrue(sumPullbackSqueezingAxes(Tensor(3)) == expected * 3)
  expectTrue(sumPullbackAlongAxes(Tensor(3)) == expected * 3)
}

TensorADTests.testAllBackends("mean") {
  let meanGradScalar = gradient { (a: Tensor<Float>) in a.mean() }
  // let meanGradSqueezingAxes = gradient { (a: Tensor<Float>) in a.mean(squeezingAxes: 0, 1) }
  let meanGradAlongAxes = gradient { (a: Tensor<Float>) in a.mean(alongAxes: 0, 1) }

  let input = Tensor<Float>(ones: [2, 2])
  let expected = Tensor<Float>(shape: [2, 2], repeating: 0.25)
  expectTrue(meanGradScalar(input) == expected)
  // expectTrue(meanGradSqueezingAxes(input) == expected)
  expectTrue(meanGradAlongAxes(input) == expected)
}

TensorADTests.testAllBackends("reshaped") {
  let shapeTensor = Tensor<Int32>([2, 2, 2])
  let input = Tensor<Float>(ones: [2, 4])
  let reshapedPullback = pullback(at: input) { (a: Tensor<Float>) in a.reshaped(toShape: shapeTensor) }
  let reshaped = Tensor<Float>(ones: [2, 2, 2])
  expectTrue(reshapedPullback(reshaped) == input)
}

TensorADTests.testAllBackends("transposed") {
  let input = Tensor<Float>(ones: [2, 3])
  let transposed = Tensor<Float>(ones: [3, 2])
  let transposedPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed() }
  let transposedPermutationsPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed(withPermutations: [1, 0]) }
  let transposedVariadicsPullback = pullback(at: input) { (a: Tensor<Float>) in a.transposed(withPermutations: 1, 0) }

  expectTrue(transposedPullback(transposed) == input)
  expectTrue(transposedPermutationsPullback(transposed) == input)
  expectTrue(transposedVariadicsPullback(transposed) == input)
}

TensorADTests.testAllBackends("relu") {
  let f = { (a: Tensor<Float>) in relu(a) }
  expectTrue([1, 0, 0] == gradient(at: [5, -5, 0], in: f))
}

TensorADTests.testAllBackends("softmax") {
  let pb = pullback(at: Tensor(ones: [2, 2])) { (a: Tensor<Float>) in softmax(a) }
  expectTrue([[0, 0], [0, 0]] == pb([[1, 1], [1, 1]]))
  expectTrue([[-0.25, 0.25], [0.75, -0.75]] == pb([[1, 2], [4, 1]]))
}

TensorADTests.testAllBackends("log_softmax") {
  let pb = pullback(at: Tensor(ones: [3, 3])) { (a: Tensor<Float>) in logSoftmax(a) }
  expectTrue(Tensor(shape: [3, 3], repeating: 5.9604645e-08) == pb(Tensor(ones: [3, 3])))
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

  // FIXME: This requires support for indirect passing.
  func bar(x: Tensor<Float>) -> Tensor<Float> {
    var a = x
    a = a * x
    a = a * x
    return a
  }
  expectEqual(Tensor(48), gradient(at: Tensor(4), in: bar))
}

TensorADTests.testAllBackends("Indirect passing 1") {
  func indirect<Scalar : Differentiable & FloatingPoint>(_ x: Tensor<Scalar>) -> Tensor<Scalar>
    where Scalar.TangentVector : AdditiveArithmetic, Scalar.CotangentVector : AdditiveArithmetic
  {
    return (x + 3) * (x + 3)
  }
  expectEqual(Tensor(8), gradient(at: Tensor(1), in: indirect))
  expectEqual(Tensor(16), pullback(at: Tensor(1), in: indirect)(Tensor(2)))
}

TensorADTests.testAllBackends("Indirect passing 2") {
  @differentiable
  func indirect(_ x: Tensor<Float>) -> Tensor<Float> {
    return x * 1 * 1 * x
  }
  expectEqual(Tensor(12), pullback(at: Tensor<Float>(3), in: indirect)(Tensor(2)))
  expectEqual(Tensor(18), pullback(at: Tensor<Float>(3), in: indirect)(Tensor(3)))
}

extension Tensor where Scalar : Differentiable & FloatingPoint,
                       Scalar.TangentVector : AdditiveArithmetic,
                       Scalar.CotangentVector : AdditiveArithmetic {
  @differentiable(vjp: vjpFoo)
  func foo(_ x: Scalar) -> Scalar {
    return x
  }
  func vjpFoo(_ x: Scalar) -> (Scalar, (Scalar.CotangentVector) -> Scalar.CotangentVector) {
    return (x, { v in v })
  }
}
TensorADTests.testAllBackends("Indirect passing 3") {
  expectEqual(Tensor(0), pullback(at: Tensor<Float>(2), in: { $0.foo(2) })(2))
  expectEqual(2.0, pullback(at: 1, in: { Tensor<Float>(1).foo($0) })(2))
  expectEqual((Tensor(0), 1), pullback(at: Tensor<Float>(1), 1, in: { $0.foo($1) })(1))
}

protocol Hi : Differentiable & FloatingPoint {
  @differentiable(wrt: (x, y))
  static func add(_ x: Self, _ y: Self) -> Self
}
extension Double : Hi {
  @differentiable(wrt: (x, y), vjp: vjpAdd)
  static func add(_ x: Double, _ y: Double) -> Double {
    return x + y
  }
  static func vjpAdd(_ x: Double, _ y: Double) -> (Double, (Double) -> (Double, Double)) {
    return (x + y, { v in (v, v) })
  }
}
TensorADTests.testAllBackends("Indirect passing 4") {
  func indirect<T : Hi>(_ x: T, _ y: T) -> (T, T) where T.TangentVector : AdditiveArithmetic,
                                                         T.CotangentVector : AdditiveArithmetic {
    return (T.add(x, x), T.add(y, 2))
  }
  expectEqual((1, 0), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).0 }))
  expectEqual((0, 1), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).1 }))
}
TensorADTests.testAllBackends("Indirect passing 5") {
  func indirect<T : Hi>(_ x: T, _ y: T) -> (T, T) where T.TangentVector : AdditiveArithmetic,
                                                         T.CotangentVector : AdditiveArithmetic {
    // FIXME: This is broken.
    let first = T.add(x, 1)
    return (T.add(first, first), T.add(y, 2))
  }
  // expectEqual((1, 0), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).0 }))
  // expectEqual((0, 1), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).1 }))
}

runAllTests()
