// RUN: %target-run-eager-swift
//
// Note: GPE testing is disabled because GPE does not interact well with
// VJP-based AD. See SR-9638.
//
// REQUIRES: executable_test
//
// FIXME(TF-199): Indirect passing differentiation crashes with `-O`.
// UNSUPPORTED: swift_test_mode_optimize
//
// Tensor indirect passing AD runtime tests.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var TensorADTests = TestSuite("TensorIndirectAD")

TensorADTests.testAllBackends("Generic") {
  func indirect<Scalar : Differentiable & FloatingPoint>(_ x: Tensor<Scalar>) -> Tensor<Scalar>
    where Scalar.TangentVector : AdditiveArithmetic, Scalar.CotangentVector : AdditiveArithmetic,
          Scalar == Scalar.CotangentVector
  {
    return (x + 3) * (x + 3)
  }
  expectEqual(Tensor(8), gradient(at: Tensor(1), in: indirect))
  expectEqual(Tensor(16), pullback(at: Tensor(1), in: indirect)(Tensor(2)))
}

TensorADTests.testAllBackends("Concrete") {
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
TensorADTests.testAllBackends("GenericMethod") {
  expectEqual(Tensor(0), pullback(at: Tensor<Float>(2), in: { $0.foo(2) })(2))
  expectEqual(2.0, pullback(at: 1, in: { Tensor<Float>(1).foo($0) })(2))
  expectEqual((Tensor(0), 1), pullback(at: Tensor<Float>(1), 1, in: { $0.foo($1) })(1))
}

// Protocol with differentiable function requirement.
protocol Addable : Differentiable & FloatingPoint {
  @differentiable(wrt: (x, y))
  static func add(_ x: Self, _ y: Self) -> Self
}
extension Double : Addable {
  @differentiable(wrt: (x, y))
  static func add(_ x: Double, _ y: Double) -> Double {
    return x + y
  }
}
TensorADTests.testAllBackends("ResultSelection") {
  func indirect<T : Addable>(_ x: T, _ y: T) -> (T, T) where T.TangentVector : AdditiveArithmetic,
                                                             T.CotangentVector : AdditiveArithmetic {
    let first = T.add(x, x)
    return (T.add(first, first), T.add(y, 2))
  }
  expectEqual((4, 0), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).0 }))
  expectEqual((0, 1), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).1 }))
}

runAllTests()
