// RUN: %target-swift-frontend -O -emit-silgen -verify %s

@differentiable(reverse, adjoint: adjointId)
func id(_ x: Float) -> Float {
  return x
}

func adjointId(_ x: Float, originalValue: Float, seed: Float) -> Float {
  return seed
}

_ = gradient(of: id)(2)

// CHECK-LABEL: @{{.*}}adjointId{{.*}}_diff_wrt_0
// CHECK-LABEL: @{{.*}}adjointId{{.*}}_diff_wrt_0_s_p
