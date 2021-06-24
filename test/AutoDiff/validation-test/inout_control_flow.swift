// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Differentiation

var InoutControlFlowTests = TestSuite("InoutControlFlow")

// SR-14218
struct Model: Differentiable {
  var first: Float = 3
  var second: Float = 1

  mutating func outer() {
    inner()
  }

  mutating func inner() {
    self.second = self.first

    // Dummy no-op if block, required to introduce control flow.
    let x = 5
    if x < 50 {}
  }
}

@differentiable(reverse)
func loss(model: Model) -> Float{
  var model = model
  model.outer()
  return model.second
}

InoutControlFlowTests.test("MutatingBeforeControlFlow") {
  var model = Model()
  let grad = gradient(at: model, of: loss)
  expectEqual(1, grad.first)
  expectEqual(0, grad.second)
}

// SR-14053
@differentiable(reverse)
func adjust(model: inout Model, multiplier: Float) {
  model.first = model.second * multiplier

  // Dummy no-op if block, required to introduce control flow.
  let x = 5
  if x < 50 {}
}

@differentiable(reverse)
func loss2(model: Model, multiplier: Float) -> Float {
  var model = model
  adjust(model: &model, multiplier: multiplier)
  return model.first
}

InoutControlFlowTests.test("InoutParameterWithControlFlow") {
  var model = Model(first: 1, second: 3)
  let grad = gradient(at: model, 5.0, of: loss2)
  expectEqual(0, grad.0.first)
  expectEqual(5, grad.0.second)
}

@differentiable(reverse)
func adjust2(multiplier: Float, model: inout Model) {
  model.first = model.second * multiplier

  // Dummy no-op if block, required to introduce control flow.
  let x = 5
  if x < 50 {}
}

@differentiable(reverse)
func loss3(model: Model, multiplier: Float) -> Float {
  var model = model
  adjust2(multiplier: multiplier, model: &model)
  return model.first
}

InoutControlFlowTests.test("LaterInoutParameterWithControlFlow") {
  var model = Model(first: 1, second: 3)
  let grad = gradient(at: model, 5.0, of: loss3)
  expectEqual(0, grad.0.first)
  expectEqual(5, grad.0.second)
}

runAllTests()
