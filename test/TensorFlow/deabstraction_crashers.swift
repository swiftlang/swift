// RUN: %target-swift-frontend -emit-sil -verify %s
import TensorFlow

// @constExpr
func one() -> Int {
  return 1
}

public func SR8369(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: one())
}
