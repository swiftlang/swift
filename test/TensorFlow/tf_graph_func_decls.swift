// RUN: %target-swift-frontend -O -emit-sil %s -verify -verify-ignore-unknown
// FIXME: Remove -verify-ignore-unknown when we fix the source location.

import TensorFlow

@TensorFlowGraph
public func innout(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + 1
}

@TensorFlowGraph
public func innout2(_ x: Tensor<Float>) -> (Tensor<Float>, Tensor<Float>) {
  return (x + 1, x + 2)
}

@inline(never)
func hostCode(_ x: Tensor<Float>) -> Tensor<Float> {
  return x.toAccelerator()
}

@TensorFlowGraph
public func usingHostCode(_ x: Tensor<Float>) -> Tensor<Float> {
  let y = x + x
  // FIXME: Expected error but unknown location "host code is not allowed
  // in a @convention(tensorflow) function"
  let z = hostCode(y).toAccelerator()
  return z + z
}

public func outer() {
  let x = Tensor<Float>([1, 2, 3])

  @TensorFlowGraph // expected-error {{@TensorFlowGraph cannot be applied to functions that capture values}}
  func inner() -> Tensor<Float> {
    return x
  }
}
