// RUN: %target-run-simple-swift
// RUN: %target-run-use-vjp-swift
// REQUIRES: executable_test

import StdlibUnittest

var SimpleModelTests = TestSuite("SimpleModel")

struct DenseLayer : Equatable {
  let w: Float
  let b: Float
}

extension DenseLayer {
  func prediction(for input: Float) -> Float {
    return input * w + b
  }
}

struct Model : Equatable {
  let l1: DenseLayer
  let l2: DenseLayer
  let l3: DenseLayer
}

extension Model {
  func prediction(for input: Float) -> Float {
    // This "model" is silly because it doesn't have nonlinearities. But it's
    // simple and good enough for testing purposes.
    let activation1 = l1.prediction(for: input)
    let activation2 = l2.prediction(for: activation1)
    return l3.prediction(for: activation2)
  }

  func loss(for input: Float, withLabel label: Float) -> Float {
    let p = prediction(for: input)
    return (p - label) * (p - label)
  }
}

SimpleModelTests.test("gradient") {
  let layer = DenseLayer(w: 1.0, b: 0.0)
  let model = Model(l1: layer, l2: layer, l3: layer)
  let input: Float = 1
  let label: Float = 3

  func loss(_ m: Model, _ i: Float, _ l: Float) -> Float {
    return m.loss(for: i, withLabel: l)
  }

  let grad = #gradient(loss, wrt: .0)(model, input, label)
  let expectedGrad = Model(l1: DenseLayer(w: -4, b: -4),
                           l2: DenseLayer(w: -4, b: -4),
                           l3: DenseLayer(w: -4, b: -4))
  expectEqual(expectedGrad, grad)
}

runAllTests()
