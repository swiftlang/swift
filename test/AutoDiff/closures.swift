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

public func closureCaptureMutable() {
  var val: Float = 10
  let clo: (Float) -> Float = { x in
    val += 2
    return val * x
  }
  _ = #gradient(clo)
}

// CHECK-LABEL: @{{.*}}closureCaptureMutable{{.*}}___adjoint_src_0_wrt_0
// CHECK: [[BOX_ADDR:%.*]] = project_box %1 : ${ var Float }, 0
// CHECK: [[BEGIN_ACCESS:%.*]] = begin_access [read] [dynamic] [[BOX_ADDR]] : $*Float
// CHECK: end_access [[BEGIN_ACCESS]] : $*Float
