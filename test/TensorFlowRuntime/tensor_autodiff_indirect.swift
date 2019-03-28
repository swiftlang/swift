// RUN: %target-run-eager-swift
//
// Note: GPE testing is disabled because GPE does not interact well with
// VJP-based AD. See SR-9638.
//
// REQUIRES: executable_test
//
// Tensor indirect passing AD runtime tests.

import TensorFlow
import StdlibUnittest
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif

var TensorADTests = TestSuite("TensorIndirectAD")

TensorADTests.testAllBackends("Generic") {
  func indirect<Scalar : TensorFlowFloatingPoint>(_ x: Tensor<Scalar>) -> Tensor<Scalar> {
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

extension Tensor where Scalar : Differentiable & FloatingPoint {
  @differentiable(wrt: x, vjp: vjpFoo)
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
  func indirect<T : Addable>(_ x: T, _ y: T) -> (T, T) {
    let first = T.add(x, x)
    return (T.add(first, first), T.add(y, 2))
  }
  expectEqual((4, 0), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).0 }))
  expectEqual((0, 1), gradient(at: Double(3), 3, in: { x, y in indirect(x, y).1 }))
}

// Mini high-level ML library.
public protocol Layer: Differentiable {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  @differentiable(wrt: (self, input))
  func applied(to input: Input) -> Output
}

@_fixed_layout
public struct Dense<Scalar: TensorFlowFloatingPoint>: Layer {
  public var weight: Tensor<Scalar>
  public var bias: Tensor<Scalar>
  public typealias Activation = @differentiable (Tensor<Scalar>) -> Tensor<Scalar>
  @noDerivative public let activation: Activation

  @differentiable(wrt: (self, input))
  public func applied(to input: Tensor<Scalar>) -> Tensor<Scalar> {
    return activation(matmul(input, weight) + bias)
  }
}

public extension Dense where Scalar.RawSignificand: FixedWidthInteger {
  init(inputSize: Int, outputSize: Int, activation: @escaping Activation) {
    self.init(weight: Tensor(
                glorotUniform: [Int32(inputSize), Int32(outputSize)]
              ),
              bias: Tensor(zeros: [Int32(outputSize)]),
              activation: activation)
  }
}

TensorADTests.testAllBackends("GenericLayerMember") {
  // Tests TF-203.
  struct GenericLayerWrapper<T: Layer> : Layer {
    var layer: T
    @differentiable(wrt: (self, input))
    func applied(to input: T.Input) -> T.Output {
      return layer.applied(to: input)
    }
  }
}

// Tests TF-235.
struct Sequential<LHS: Layer, RHS: Layer>: Layer
  where LHS.Output == RHS.Input
{
  let lhs: LHS
  let rhs: RHS

  init(_ lhs: LHS, _ rhs: RHS) {
    self.lhs = lhs
    self.rhs = rhs
  }

  @differentiable(wrt: (self, input))
  func applied(to input: LHS.Input) -> RHS.Output {
    let intermediateValue = lhs.applied(to: input)
    return rhs.applied(to: intermediateValue)
  }
}

// Forward function composition operator. Used in F#.
func >> <LHS: Layer, RHS: Layer>(_ lhs: LHS, _ rhs: RHS) -> Sequential<LHS, RHS> {
  return Sequential(lhs, rhs)
}

struct LayerTriple<T: Layer, U: Layer, V : Layer>: Layer
  where T.Output == U.Input, U.Output == V.Input
{
  let first: T
  let second: U
  let third: V

  init(_ first: T, _ second: U, _ third: V) {
    self.first = first
    self.second = second
    self.third = third
  }

  @differentiable(wrt: (self, input))
  func applied(to input: T.Input) -> V.Output {
    let intermediate1 = first.applied(to: input)
    let intermediate2 = second.applied(to: intermediate1)
    return third.applied(to: intermediate2)
  }
}

// Tests TF-235 and TF-242.
TensorADTests.testAllBackends("GenericLayerMembers") {
  func testFixedInput() {
    let lhs = Dense<Float>(inputSize: 3, outputSize: 4, activation: relu)
    let rhs = Dense<Float>(inputSize: 4, outputSize: 5, activation: sigmoid)
    let combined = Sequential(lhs, rhs)

    let input = Tensor<Float>(ones: [2, 3])
    let seed = Tensor<Float>(ones: [input.shape[0], rhs.weight.shape[1]])
    let (𝛁lhs, 𝛁rhs) = pullback(at: lhs, rhs) { lhs, rhs in
      rhs.applied(to: lhs.applied(to: input))
    }(seed)
    let 𝛁combined = pullback(at: combined) { $0.applied(to: input) }(seed)
    expectEqual(Sequential.CotangentVector(lhs: 𝛁lhs, rhs: 𝛁rhs), 𝛁combined)
  }
  testFixedInput()

  func testWrtInput(_ input: Tensor<Float>) {
    let lhs = Dense<Float>(inputSize: 3, outputSize: 4, activation: relu)
    let rhs = Dense<Float>(inputSize: 4, outputSize: 5, activation: sigmoid)
    // Test "compose" operator.
    let combined = lhs >> rhs

    let seed = Tensor<Float>(ones: [input.shape[0], rhs.weight.shape[1]])
    let (𝛁lhs, 𝛁rhs) = pullback(at: lhs, rhs) { lhs, rhs in
      rhs.applied(to: lhs.applied(to: input))
    }(seed)
    let 𝛁combined = pullback(at: combined) { $0.applied(to: input) }(seed)
    expectEqual(Sequential.CotangentVector(lhs: 𝛁lhs, rhs: 𝛁rhs), 𝛁combined)
  }
  testWrtInput(Tensor(randomUniform: [2, 3]))
}

// Tests TF-308.
TensorADTests.testAllBackends("GenericWrapperLayer") {
  struct Wrapper<Wrapped: Layer, Scalar: TensorFlowFloatingPoint>: Layer
    where Wrapped.Input == Tensor<Scalar>, Wrapped.Output == Tensor<Scalar>
  {
    var layer: Wrapped

    @differentiable
    func applied(to input: Wrapped.Input) -> Wrapped.Output {
      return layer.applied(to: input)
    }
  }

  let dense = Dense<Float>(inputSize: 2, outputSize: 3, activation: relu)
  let wrapper = Wrapper(layer: dense)
  let input = Tensor<Float>(ones: [2, 2])
  let seed = Tensor<Float>(ones: [input.shape[0], dense.weight.shape[1]])

  let 𝛁wrapper = pullback(at: wrapper) { $0.applied(to: input) }(seed)
  let 𝛁dense = pullback(at: dense) { $0.applied(to: input) }(seed)
  expectEqual(Wrapper.CotangentVector(layer: 𝛁dense), 𝛁wrapper)
}

runAllTests()
