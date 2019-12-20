// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var SimpleModelTests = TestSuite("SimpleModel")

struct DenseLayer : Equatable {
  @differentiable
  let w: Tracked<Float>

  @differentiable
  let b: Tracked<Float>
}

extension DenseLayer : Differentiable, AdditiveArithmetic {
  typealias TangentVector = DenseLayer
  typealias Scalar = Tracked<Float>
  static var zero: DenseLayer {
    return DenseLayer(w: 0, b: 0)
  }
  static func + (lhs: DenseLayer, rhs: DenseLayer) -> DenseLayer {
    return DenseLayer(w: lhs.w + rhs.w, b: lhs.b + rhs.b)
  }
  static func - (lhs: DenseLayer, rhs: DenseLayer) -> DenseLayer {
    return DenseLayer(w: lhs.w - rhs.w, b: lhs.b - rhs.b)
  }
  static func * (lhs: DenseLayer, rhs: DenseLayer) -> DenseLayer {
    return DenseLayer(w: lhs.w * rhs.w, b: lhs.b * rhs.b)
  }
  static func * (lhs: Tracked<Float>, rhs: DenseLayer) -> DenseLayer {
    return DenseLayer(w: lhs * rhs.w, b: lhs * rhs.b)
  }
}

extension DenseLayer {
  func prediction(for input: Tracked<Float>) -> Tracked<Float> {
    return input * w + b
  }
}

struct Model : Equatable {
  @differentiable
  let l1: DenseLayer

  @differentiable
  let l2: DenseLayer

  @differentiable
  let l3: DenseLayer
}

extension Model : Differentiable, AdditiveArithmetic {
  typealias TangentVector = Model
  typealias Scalar = Tracked<Float>
  static var zero: Model {
    return Model(l1: DenseLayer.zero, l2: DenseLayer.zero, l3: DenseLayer.zero)
  }
  static func + (lhs: Model, rhs: Model) -> Model {
    return Model(l1: lhs.l1 + rhs.l1, l2: lhs.l2 + rhs.l2, l3: lhs.l3 + rhs.l3)
  }
  static func - (lhs: Model, rhs: Model) -> Model {
    return Model(l1: lhs.l1 - rhs.l1, l2: lhs.l2 - rhs.l2, l3: lhs.l3 - rhs.l3)
  }
  static func * (lhs: Model, rhs: Model) -> Model {
    return Model(l1: lhs.l1 * rhs.l1, l2: lhs.l2 * rhs.l2, l3: lhs.l3 * rhs.l3)
  }
  static func * (lhs: Tracked<Float>, rhs: Model) -> Model {
    return Model(l1: lhs * rhs.l1, l2: lhs * rhs.l2, l3: lhs * rhs.l3)
  }
}

extension Model {
  func prediction(for input: Tracked<Float>) -> Tracked<Float> {
    // This "model" is silly because it doesn't have nonlinearities. But it's
    // simple and good enough for testing purposes.
    let activation1 = l1.prediction(for: input)
    let activation2 = l2.prediction(for: activation1)
    return l3.prediction(for: activation2)
  }

  func loss(of prediction: Tracked<Float>, from label: Tracked<Float>) -> Tracked<Float> {
    return (prediction - label) * (prediction - label)
  }
}

SimpleModelTests.testWithLeakChecking("gradient") {
  let layer = DenseLayer(w: 1.0, b: 0.0)
  let model = Model(l1: layer, l2: layer, l3: layer)
  let label: Tracked<Float> = 3
  let input: Tracked<Float> = 1
  let gradModel = gradient(at: model) { model -> Tracked<Float> in
    let pred = model.prediction(for: input)
    return model.loss(of: pred, from: label)
  }
  let expectedGrad = Model(l1: DenseLayer(w: -4, b: -4),
                           l2: DenseLayer(w: -4, b: -4),
                           l3: DenseLayer(w: -4, b: -4))
  expectEqual(expectedGrad, gradModel)
}

runAllTests()
