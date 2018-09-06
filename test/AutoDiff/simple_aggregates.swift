// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SimpleAggregateTests = TestSuite("SimpleAggregate")

struct DenseLayer : Equatable {
  let w: Float
  let b: Float
}

// TODO: Deduplicate these when it's possible.
func prediction1(_ model: DenseLayer, _ input: Float) -> Float {
  return input * model.w + model.b
}
func prediction2(_ model: DenseLayer, _ input: Float) -> Float {
  return input * model.w + model.b
}
func prediction3(_ model: DenseLayer, _ input: Float) -> Float {
  return input * model.w + model.b
}

struct Model : Equatable {
  let l1: DenseLayer
  let l2: DenseLayer
  let l3: DenseLayer
}

func prediction(_ model: Model, _ input: Float) -> Float {
  // This "model" is silly because it doesn't have nonlinearities. But it's
  // simple and good enough for testing purposes.
  let l1 = prediction1(model.l1, input)
  let l2 = prediction2(model.l2, l1)
  return prediction3(model.l3, l2)
}

func loss(_ model: Model, _ input: Float, _ label: Float) -> Float {
  let p = prediction(model, input)
  return (p - label) * (p - label)
}

SimpleAggregateTests.test("gradient") {
  let layer = DenseLayer(w: 1.0, b: 0.0)
  let model = Model(l1: layer, l2: layer, l3: layer)
  let input: Float = 1
  let label: Float = 3

  let grad = #gradient(loss, wrt: .0)(model, input, label)
  let expectedGrad = Model(l1: DenseLayer(w: -4, b: -4),
                           l2: DenseLayer(w: -4, b: -4),
                           l3: DenseLayer(w: -4, b: -4))
  expectEqual(expectedGrad, grad)
}

runAllTests()
