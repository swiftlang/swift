// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-check-deabstraction -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -Xllvm -tf-check-deabstraction -verify %s | %FileCheck %s
import TensorFlow

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

