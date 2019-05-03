// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown
// REQUIRES: tensorflow

import TensorFlow

// Synthesis should work for empty structs.
// `allKeyPaths` simply returns `[]`.
struct Empty : KeyPathIterable {}

struct Parameters : KeyPathIterable {
  var w: Float
  var b: Float
}
var params = Parameters(w: 1, b: 2)
assert(params.allKeyPaths.count == 2)
assert(params.allKeyPaths(to: Float.self).count == 2)
assert(params.allKeyPaths(to: Int.self).count == 0)
for kp in params.allWritableKeyPaths(to: Float.self) {
  params[keyPath: kp] *= 2
}

struct TensorParameters : KeyPathIterable {
  var w: Tensor<Float>
  var b: Tensor<Float>

  // Non-stored-property members should not affect synthesis.
  var computed: Float {
    return (w + b).scalarized()
  }
  func foo() {}
  typealias Foo = Int
}

extension TensorParameters : VectorNumeric {
  static var zero: TensorParameters {
    return TensorParameters(w: Tensor(0), b: Tensor(0))
  }
  static func + (lhs: TensorParameters, rhs: TensorParameters) -> TensorParameters {
    return TensorParameters(w: lhs.w + rhs.w, b: lhs.b + rhs.b)
  }
  static func - (lhs: TensorParameters, rhs: TensorParameters) -> TensorParameters {
    return TensorParameters(w: lhs.w + rhs.w, b: lhs.b + rhs.b)
  }
  typealias Scalar = Tensor<Float>
  static func * (lhs: Scalar, rhs: TensorParameters) -> TensorParameters { 
    return TensorParameters(w: lhs + rhs.w, b: lhs + rhs.b)
  }
}

struct HeterogeneousParameters : KeyPathIterable {
  var float: Float
  var double: Double
  var tensor: Tensor<Float>
  var params: Parameters
}
let hetero = HeterogeneousParameters(float: 0, double: 0,
                                     tensor: Tensor(0), params: params)
assert(hetero.allKeyPaths.count == 4)
assert(hetero.recursivelyAllKeyPaths.count == 6)
assert(hetero.allKeyPaths(to: Float.self).count == 1)
assert(hetero.recursivelyAllKeyPaths(to: Float.self).count == 3)
assert(hetero.allKeyPaths(to: Tensor<Float>.self).count == 1)
assert(hetero.allKeyPaths(to: Parameters.self).count == 1)
assert(hetero.allKeyPaths(to: Int.self).count == 0)

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

// `pow` is defined in Darwin on `Float` and `Double`, but there doesn't exist
// a generic version for `FloatingPoint`.
// This is a manual definition.
func pow<T : BinaryFloatingPoint>(_ x: T, _ y: T) -> T {
  // return T(pow(Float(x), Float(y)))
  return T(pow(Double(x), Double(y)))
}

struct AdamOptimizer<P : KeyPathIterable, Scalar : BinaryFloatingPoint>
  where P : VectorNumeric, P.Scalar == Tensor<Scalar>
{
  let learningRate: Scalar
  var beta1: Scalar
  var beta2: Scalar
  var epsilon: Scalar

  init(
    learningRate: Scalar = 1e-3,
    beta1: Scalar = 0.9,
    beta2: Scalar = 0.999,
    epsilon: Scalar = 1e-8
  ) {
    self.learningRate = learningRate
    self.beta1 = beta1
    self.beta2 = beta2
    self.epsilon = epsilon
  }

  var step: Scalar = 0
  var firstMoments: P = P.zero
  var secondMoments: P = P.zero

  mutating func fitParameters(
    parameters: inout P, withGradients gradients: P
  ) {
    for kp in parameters.recursivelyAllWritableKeyPaths(to: Tensor<Scalar>.self) {
      firstMoments[keyPath: kp] =
        firstMoments[keyPath: kp] * beta1 + (1 - beta1) * gradients[keyPath: kp]
      secondMoments[keyPath: kp] =
        firstMoments[keyPath: kp] * beta2 + (1 - beta2) * gradients[keyPath: kp] * gradients[keyPath: kp]

      let denominator = sqrt(secondMoments[keyPath: kp]) + epsilon
      step += 1
      let biasCorrection1 = 1 - pow(beta1, step)
      let biasCorrection2 = 1 - pow(beta2, step)
      let stepSize = learningRate * sqrt(biasCorrection2) / biasCorrection1
      parameters[keyPath: kp] -= stepSize * firstMoments[keyPath: kp] / denominator
    }
  }
}

func testOptimizer<P : KeyPathIterable, Scalar : BinaryFloatingPoint>(
  parameters: inout P, withGradients gradients: P
)
  where P : VectorNumeric, P.Scalar == Tensor<Scalar>
{
  var optimizer = AdamOptimizer<P, Scalar>()
  print(parameters)
  for _ in 0..<5 {
    optimizer.fitParameters(parameters: &parameters, withGradients: gradients)
    print(parameters)
  }
}
var tensorParams = TensorParameters.zero
let gradients = TensorParameters(w: Tensor(10), b: Tensor(10))
testOptimizer(parameters: &tensorParams, withGradients: gradients)
