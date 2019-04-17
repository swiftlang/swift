// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SimpleModelTests = TestSuite("SimpleModel")

protocol Layer : Differentiable, VectorNumeric {
  @differentiable
  call func(_ input: Float) -> Float
}

struct DenseLayer : Layer {
  let w, b: Float

  @differentiable
  call func(_ input: Float) -> Float {
    return input * w + b
  }
}

struct Model : Layer {
  let l1, l2, l3: DenseLayer

  @differentiable
  call func(_ input: Float) -> Float {
    // This model is silly because it has no nonlinearities.
    // But it is simple and good enough for testing purposes.
    return l3(l2(l1(input)))
  }
}

extension Model {
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
    let prediction = model(input)
    return model.loss(of: prediction, from: label)
  }
  let expectedGrad = Model(l1: DenseLayer(w: -4, b: -4),
                           l2: DenseLayer(w: -4, b: -4),
                           l3: DenseLayer(w: -4, b: -4))
  expectEqual(expectedGrad, gradModel)
}

runAllTests()
