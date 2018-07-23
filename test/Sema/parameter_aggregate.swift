// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown
// REQUIRES: tensorflow

import TensorFlow

struct Parameters : ParameterAggregate {
  var w: Float
  var b: Float
}
var params = Parameters(w: 1, b: 2)
params.update(withGradients: params, -=)
params.update(withGradients: params) { p, g in p -= g }
params.update(withGradients: params) { $0 -= 0.1 * $1 }

struct TensorParameters : ParameterAggregate {
  var w: Tensor<Float>
  var b: Tensor<Float>

  // Non-stored-property members should not affect synthesis.
  var computed: Float {
    return (w + b).scalarized()
  }
  func foo() {}
  typealias Foo = Int
}
var tensorParams = TensorParameters(w: [[1, 1]], b: [2])
tensorParams.update(withGradients: tensorParams, -=)
tensorParams.update(withGradients: tensorParams) { p, g in p -= g }
tensorParams.update(withGradients: tensorParams) { $0 -= 0.1 * $1 }

// Test types with ParameterAggregate members.

struct NestedParameters : ParameterAggregate {
  var params1: Parameters
  var params2: Parameters
  var float: Float
}
var nested = NestedParameters(params1: params, params2: params, float: 1)
nested.update(withGradients: nested, -=)
nested.update(withGradients: nested) { p, g in p -= g }
nested.update(withGradients: nested) { $0 -= 0.1 * $1 }

struct VeryNestedParameters : ParameterAggregate {
  var nested: NestedParameters
  var params: Parameters
  var float: Float
}
var veryNested = VeryNestedParameters(nested: nested, params: params, float: 1)
veryNested.update(withGradients: veryNested, -=)
veryNested.update(withGradients: veryNested) { p, g in p -= g }
veryNested.update(withGradients: veryNested) { $0 -= 0.1 * $1 }

// Test type in generic context.

struct A<T> {
  struct B<U, V> {
    struct GenericContextParams : ParameterAggregate {
      var params: Parameters
      var float: Float
    }
  }
}

// Test manual conformances to ParameterAggregate.

struct HeterogeneousParameters : ParameterAggregate {
  var float: Float
  var double: Double

  typealias Parameter = Float

  mutating func update(
    withGradients gradients: HeterogeneousParameters,
    _ updater: (inout Float, Float) -> Void
  ) {
    updater(&float, gradients.float)

    var tmp = Float(double)
    updater(&tmp, Float(gradients.double))
    double = Double(tmp)
  }
}
