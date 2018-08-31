// RUN: %target-swift-frontend -O -emit-llvm %s | %FileCheck %s
// XFAIL: *

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
