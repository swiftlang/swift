// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown
// REQUIRES: tensorflow

import TensorFlow

struct DenseLayer : Parameterized {
  @TFParameter var w: Float
  @TFParameter var b: Float
}

var layer = DenseLayer(w: 1, b: 2)
let layerGradients = DenseLayer.Parameters(w: 1, b: 2)
layer.updateParameters(withGradients: layer.allParameters, -=)
layer.updateParameters(withGradients: layerGradients) { p, g in p -= g }
layer.updateParameters(withGradients: layerGradients) { $0 -= 0.1 * $1 }

struct TensorLayer : Parameterized {
  @TFParameter var w: Tensor<Float>
  @TFParameter var b: Tensor<Float>

  // Members that are not stored properties marked with @TFParameter should
  // not affect synthesis.
  var stored: Tensor<Float>
  var computed: Float {
    return (w + b).scalarized()
  }
  func foo() {}
  typealias Foo = Int
}

_ = TensorLayer.Parameters(w: [1], b: [2])

// Test types with `Parameterized` members.
struct Model : Parameterized {
  @TFParameter var layer1: DenseLayer
  @TFParameter var layer2: DenseLayer
  @TFParameter var float: Float
  // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
  func moved(along direction: Parameters) -> Model {
    return self // incorrect dummy implementation to pass type checking
  }
  func tangentVector(from cotangent: Parameters) -> Parameters {
    return cotangent
  }
}

var model = Model(layer1: layer, layer2: layer, float: 1)
let modelGradients = Model.Parameters(layer1: layer.allParameters,
                                      layer2:layer.allParameters,
                                      float: 1)
model.updateParameters(withGradients: model.allParameters, -=)
model.updateParameters(withGradients: modelGradients) { p, g in p -= g }
model.updateParameters(withGradients: modelGradients) { $0 -= 0.1 * $1 }

// Test extension of synthesized `Parameters` struct.
extension Model.Parameters {
  func foo() {}
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextModel : Parameterized {
      @TFParameter var layer: DenseLayer
      @TFParameter var float: Float
      // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
      func moved(along direction: Parameters) -> GenericContextModel {
        return self // incorrect dummy implementation to pass type checking
      }
      func tangentVector(from cotangent: Parameters) -> Parameters {
        return cotangent
      }
    }
  }
}

func foo() {
  struct NestedInFunction : Parameterized {
    @TFParameter var layer: DenseLayer
    @TFParameter var float: Float
    // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
    func moved(along direction: Parameters) -> NestedInFunction {
      return self // incorrect dummy implementation to pass type checking
    }
    func tangentVector(from cotangent: Parameters) -> Parameters {
      return cotangent
    }
  }
}

// Test heterogenous parameter types.
struct MixedParameterized : Parameterized {
  @TFParameter var int: Int
  @TFParameter var float: Float
  @TFParameter var string: String
  // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
  func moved(along direction: Parameters) -> MixedParameterized {
    return self // incorrect dummy implementation to pass type checking
  }
  func tangentVector(from cotangent: Parameters) -> Parameters {
    return cotangent
  }
}
var mixed = MixedParameterized(int: 1, float: 3.14, string: "foo")
_ = mixed.allParameters

extension MixedParameterized.Parameters : ParameterGroup {
  typealias Parameter = Float
  mutating func update(
    withGradients gradients: MixedParameterized.Parameters,
    _ updater: (inout Float, Float) -> Void
  ) {
    // Dummy empty body.
  }
}
mixed.updateParameters(withGradients: mixed.allParameters, -=)

// Test user-specified `Parameters` struct with reordered parameters.
struct ModelWithReorderedParameters : Parameterized {
  @TFParameter var layer: DenseLayer
  @TFParameter var float: Float
  struct Parameters : ParameterGroup {
    var float: Float
    var layer: DenseLayer.Parameters
  }
  // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
  func moved(along direction: Parameters) -> ModelWithReorderedParameters {
    return self // incorrect dummy implementation to pass type checking
  }
  func tangentVector(from cotangent: Parameters) -> Parameters {
    return cotangent
  }
}
var reorderedModel = ModelWithReorderedParameters(layer: layer, float: 1)
_ = reorderedModel.allParameters
reorderedModel.updateParameters(withGradients: reorderedModel.allParameters, -=)

// Test invalid `Parameters` struct.
struct ModelWithInvalidParameters : Parameterized {
  @TFParameter var layer: DenseLayer
  @TFParameter var float: Float
  struct Parameters {} // expected-error {{'Parameters' struct is invalid}}
  // TODO: Remove when `moved(along:)` and `tangentVector(from:)` are reliably synthesized.
  func moved(along direction: Parameters) -> ModelWithInvalidParameters {
    return self // incorrect dummy implementation to pass type checking
  }
  func tangentVector(from cotangent: Parameters) -> Parameters {
    return cotangent
  }
}

// Test invalid `@TFParameter` usage.
struct InvalidTFParameterUsage : Parameterized {
  @TFParameter var layer: DenseLayer
  @TFParameter var float: Float

  @TFParameter var computed: Float { return 1 } // expected-error {{only instance stored properties can be declared @TFParameter}}
  @TFParameter func method() -> Float { return 1 } // expected-error {{@TFParameter may only be used on 'var' declarations}}
  @TFParameter struct MemberStruct {} // expected-error {{@TFParameter may only be used on 'var' declarations}}
}

// Test invalid `@TFParameter` usage outside of `Parameterized` type.
struct NonParameterized {
  @TFParameter var float: Float // expected-error {{@TFParameter is allowed only in types that conform to 'Parameterized'}}
}
