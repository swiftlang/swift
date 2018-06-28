// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt -differentiation -sil-print-all -verify | %FileCheck %s

// TODO(SWIFT-8146): Re-enable this test.
// UNSUPPORTED: tensorflow

import TensorFlow

@differentiable(reverse, adjoint: dConcreteTanh)
func concreteTanh(_ x: Tensor<Float>) -> Tensor<Float> {
  return tanh(x)
}

func dConcreteTanh(_ x: Tensor<Float>, tanhx: Tensor<Float>, seed: Tensor<Float>) -> Tensor<Float> {
  return seed * (1 - tanhx * tanhx)
}

_ = #gradient(concreteTanh)([1,2,3,4,5])

// CHECK: @{{.*}}concreteTanh{{.*}}__grad_src_0_wrt_0_s_p
// CHECK: @{{.*}}concreteTanh{{.*}}__grad_src_0_wrt_0
