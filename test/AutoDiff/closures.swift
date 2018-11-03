// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

public func closureCapture() {
  let val: Float = 10
  let clo: (Float) -> Float = { x in
    val * x
  }
  _ = #gradient(clo)
}

// CHECK-LABEL: @{{.*}}closureCapture{{.*}}
// CHECK: [[ORIG_FN:%.*]] = function_ref @{{.*}}closureCapture{{.*}}
// CHECK: [[PARTIAL_APPLIED:%.*]] = partial_apply [callee_guaranteed] [[ORIG_FN]]
// CHECK: [[GRAD_FN:%.*]] = function_ref @{{.*}}closureCapture{{.*}}___grad
// CHECK: [[PARTIAL_APPLIED_GRAD:%.*]] = partial_apply [callee_guaranteed] [[GRAD_FN]]
