// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SimpleModelTests = TestSuite("SimpleModel")

struct DenseLayer : Equatable {
  @differentiable(wrt: (self), vjp: vjpW)
  let w: Float
  func vjpW() -> (Float, (Float) -> DenseLayer) {
    return (w, { dw in DenseLayer(w: dw, b: 0) } )
  }

  @differentiable(wrt: (self), vjp: vjpB)
  let b: Float
  func vjpB() -> (Float, (Float) -> DenseLayer) {
    return (b, { db in DenseLayer(w: 0, b: db) } )
  }
}

extension DenseLayer : Differentiable, VectorNumeric {
  typealias TangentVector = DenseLayer
  typealias CotangentVector = DenseLayer
  typealias Scalar = Float
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
  static func * (lhs: Float, rhs: DenseLayer) -> DenseLayer {
    return DenseLayer(w: lhs * rhs.w, b: lhs * rhs.b)
  }
}

extension DenseLayer {
  func prediction(for input: Float) -> Float {
    return input * w + b
  }
}

struct Model : Equatable {
  @differentiable(wrt: (self), vjp: vjpL1)
  let l1: DenseLayer
  func vjpL1() -> (DenseLayer, (DenseLayer) -> Model) {
    return (l1, { dl1 in Model(l1: dl1, l2: DenseLayer.zero, l3: DenseLayer.zero) } )
  }

  @differentiable(wrt: (self), vjp: vjpL2)
  let l2: DenseLayer
  func vjpL2() -> (DenseLayer, (DenseLayer) -> Model) {
    return (l2, { dl2 in Model(l1: DenseLayer.zero, l2: dl2, l3: DenseLayer.zero) } )
  }

  @differentiable(wrt: (self), vjp: vjpL3)
  let l3: DenseLayer
  func vjpL3() -> (DenseLayer, (DenseLayer) -> Model) {
    return (l3, { dl3 in Model(l1: DenseLayer.zero, l2: DenseLayer.zero, l3: dl3) } )
  }
}

extension Model : Differentiable, VectorNumeric {
  typealias TangentVector = Model
  typealias CotangentVector = Model
  typealias Scalar = Float
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
  static func * (lhs: Float, rhs: Model) -> Model {
    return Model(l1: lhs * rhs.l1, l2: lhs * rhs.l2, l3: lhs * rhs.l3)
  }
}

extension Model {
  func prediction(for input: Float) -> Float {
    // This "model" is silly because it doesn't have nonlinearities. But it's
    // simple and good enough for testing purposes.
    let activation1 = l1.prediction(for: input)
    let activation2 = l2.prediction(for: activation1)
    return l3.prediction(for: activation2)
  }

  func loss(of prediction: Float, from label: Float) -> Float {
    return (prediction - label) * (prediction - label)
  }
}

SimpleModelTests.test("gradient") {
  let layer = DenseLayer(w: 1.0, b: 0.0)
  let model = Model(l1: layer, l2: layer, l3: layer)
  let label: Float = 3
  let input: Float = 1
  let gradModel = model.gradient { model -> Float in
    let pred = model.prediction(for: input)
    return model.loss(of: pred, from: label)
  }
  let expectedGrad = Model(l1: DenseLayer(w: -4, b: -4),
                           l2: DenseLayer(w: -4, b: -4),
                           l3: DenseLayer(w: -4, b: -4))
  expectEqual(expectedGrad, gradModel)
}

runAllTests()
