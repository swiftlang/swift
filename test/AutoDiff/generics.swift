// RUN: %target-swift-frontend -emit-sil -verify %s

struct Tensor<T : VectorNumeric> : VectorNumeric, Differentiable {
  var value: Float
  init(_ value: Float) { self.value = value }
}

func generic<T : FloatingPoint & Differentiable>(_ x: Tensor<T>) -> Float {
  return x.value + x.value
}
print(pullback(at: Tensor<Float>(1), in: generic))
print(pullback(at: Tensor<Float>(3), in: generic))

// TODO: add more tests.
