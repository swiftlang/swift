// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_key_path_iterable_other_module.swift

struct Tensor<Scalar> {
  var scalar: Scalar
  init(_ scalar: Scalar) {
    self.scalar = scalar
  }
}
extension Tensor : Equatable where Scalar : Equatable {}
extension Tensor : AdditiveArithmetic where Scalar : AdditiveArithmetic {}
extension Tensor : VectorProtocol where Scalar : AdditiveArithmetic {
  typealias VectorSpaceScalar = Scalar
  func scaled(by scalar: Scalar) -> Self { self }
}

// Synthesis should work for empty structs.
// `allKeyPaths` simply returns `[]`.
struct Empty : KeyPathIterable {}

struct Parameters : KeyPathIterable {
  var w: Float
  var b: Float
}
func testParameters() {
  var params = Parameters(w: 1, b: 2)
  assert(params.allKeyPaths.count == 2)
  assert(params.allKeyPaths(to: Float.self).count == 2)
  assert(params.allKeyPaths(to: Int.self).count == 0)
  for kp in params.allWritableKeyPaths(to: Float.self) {
    params[keyPath: kp] *= 2
  }
}

struct TensorParameters : KeyPathIterable {
  var w: Tensor<Float>
  var b: Tensor<Float>

  // Non-stored-property members should not affect synthesis.
  var computed: Float {
    return (w + b).scalar
  }
  func foo() {}
  typealias Foo = Int
}

extension TensorParameters : VectorProtocol {
  static var zero: TensorParameters {
    return TensorParameters(w: Tensor(0), b: Tensor(0))
  }
  static func + (lhs: TensorParameters, rhs: TensorParameters) -> TensorParameters {
    return TensorParameters(w: lhs.w + rhs.w, b: lhs.b + rhs.b)
  }
  static func - (lhs: TensorParameters, rhs: TensorParameters) -> TensorParameters {
    return TensorParameters(w: lhs.w + rhs.w, b: lhs.b + rhs.b)
  }
  typealias VectorSpaceScalar = Float
  func scaled(by scalar: VectorSpaceScalar) -> TensorParameters {
    return TensorParameters(w: w.scaled(by: scalar), b: b.scaled(by: scalar))
  }
}

struct HeterogeneousParameters : KeyPathIterable {
  var float: Float
  var double: Double
  var tensor: Tensor<Float>
  var params: Parameters
}
func testHeterogenousParameters(_ params: Parameters) {
  let hetero = HeterogeneousParameters(float: 0, double: 0,
                                       tensor: Tensor(0), params: params)
  assert(hetero.allKeyPaths.count == 4)
  assert(hetero.recursivelyAllKeyPaths.count == 6)
  assert(hetero.allKeyPaths(to: Float.self).count == 1)
  assert(hetero.recursivelyAllKeyPaths(to: Float.self).count == 3)
  assert(hetero.allKeyPaths(to: Tensor<Float>.self).count == 1)
  assert(hetero.allKeyPaths(to: Parameters.self).count == 1)
  assert(hetero.allKeyPaths(to: Int.self).count == 0)
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextParams : KeyPathIterable {
      var params: Parameters
      var float: Float
    }
  }
}

// Test generic optimizer.

struct DummyOptimizer<P : KeyPathIterable, Scalar : BinaryFloatingPoint>
  where P : VectorProtocol, P.VectorSpaceScalar == Scalar
{
  let learningRate: Scalar
  var firstMoments: P = P.zero

  mutating func fitParameters(
    parameters: inout P, withGradients gradients: P
  ) {
    for kp in parameters.recursivelyAllWritableKeyPaths(to: Tensor<Scalar>.self) {
      firstMoments[keyPath: kp] *= learningRate
      parameters[keyPath: kp] -= learningRate * parameters[keyPath: kp]
    }
  }
}

// Test derived conformances in disallowed contexts.

// expected-error @+2 {{type 'OtherFileNonconforming' does not conform to protocol 'KeyPathIterable'}}
// expected-error @+1 {{implementation of 'KeyPathIterable' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : KeyPathIterable {}

// expected-error @+2 {{type 'GenericOtherFileNonconforming<T>' does not conform to protocol 'KeyPathIterable'}}
// expected-error @+1 {{implementation of 'KeyPathIterable' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : KeyPathIterable {}
