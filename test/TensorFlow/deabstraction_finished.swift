// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-strict-deabstraction -verify %s | %FileCheck %s
import TensorFlow


public func trivialAdd(a: Tensor<Float>) -> Tensor<Float> {
  let b = a.toAccelerator()
  return b+b
}

// @constExpr
func one() -> Int {
  return 1
}

public func constexprCall(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: one())
}



struct Wrapper {
  let v : Int
}

public func f(a: Tensor<Float>, idx: Tensor<Int32>) -> Tensor<Float> {
  let w = Wrapper(v: 1)
  return Tensor<Float>(oneHotAtIndices: idx.toAccelerator(), depth: 0, axis: w.v)
}




// FIXME: Constexpr propagation of tensorshape should handle this.
public func tensorShape() -> Tensor<Float> {
  let shape : TensorShape = [2]
  // expected-error @+1 {{attribute 'value' requires a constant argument}}
  return Tensor(handle: #tfop("Const", dtype: Float.self, value$tensor: [1.0, 2.0], value$shape: shape))
}

// b/75407624

// This requires propagation of the array initializer of TensorShape through its
// initializers.
public func test75407624() {
    let a = Tensor<Float>([1])
    let b = Tensor<Float>(shape: [1], repeating: 1)
    let c = Tensor<Float>(shape: [1], repeating: 1)
    _ = a+b+c
}


/*
CHECK-LABEL: --- TFDeabstraction Result: {{.*}}test75407624

CHECK: [[TFROM1D:%.*]] = function_ref @__tf_tensor_from_scalars_1d

// FIXME: This is actually a failure.  This function should be promoted.
CHECK: apply [[TFROM1D]]

CHECK: [[TFROM1D:%.*]] = function_ref @__tf_tensor_from_scalars_1d

// FIXME: This is actually a failure.  This function should be promoted.
CHECK: apply [[TFROM1D]]

CHECK-LABEL: ----
*/


