// RUN: %target-swift-frontend -emit-sil -verify %s
import TensorFlow

public func SR8299(a: Tensor<Float>) {
  // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type 'Tensor<Float>'}}
   () = #tfop("foo", someAttr: a)
}

// @constExpr
func one() -> Int {
  return 1
}

public func SR8369(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: one())
}
