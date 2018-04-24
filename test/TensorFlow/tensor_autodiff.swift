// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt -differentiation -sil-print-all -verify | %FileCheck %s

import TensorFlow

@differentiable(reverse, adjoint: dConcreteTanh)
func concreteTanh(_ x: Tensor<Float>) -> Tensor<Float> {
  return tanh(x)
}

func dConcreteTanh(_ x: Tensor<Float>, tanhx: Tensor<Float>, seed: Tensor<Float>) -> Tensor<Float> {
  return seed.broadcast(to: tanhx) * (1 - tanhx * tanhx)
}

_ = gradient(of: concreteTanh)([1,2,3,4,5])

// CHECK: @{{.*}}concreteTanh{{.*}}__grad_wrt_0_s_p
// CHECK: @{{.*}}concreteTanh{{.*}}__grad_wrt_0
