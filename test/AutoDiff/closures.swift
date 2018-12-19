// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

public func closureCapture() {
  let val: Float = 10
  let clo: (Float) -> Float = { x in
    val * x
  }
  _ = gradient(at: 0, in: clo)
}

// CHECK-LABEL: @{{.*}}closureCapture{{.*}}___vjp_src_0_wrt_0
// CHECK: bb0({{.*}} : $Float, [[CAPTURE:%.*]] : $Float):
// CHECK:   [[PRIMAL:%.*]] = function_ref @AD__{{.*}}closureCapture{{.*}}___primal_src_0_wrt_0
// CHECK:   apply [[PRIMAL]]({{.*}}, [[CAPTURE]])
// CHECK:   [[ADJOINT:%.*]] = function_ref @AD__{{.*}}closureCapture{{.*}}___adjoint_src_0_wrt_0
// CHECK:   partial_apply [callee_guaranteed] [[ADJOINT]]({{.*}}, {{.*}}, {{.*}}, [[CAPTURE]])

public func closureCaptureMutable() {
  var val: Float = 10
  let clo: (Float) -> Float = { x in
    val += 2
    return val * x
  }
  _ = gradient(at: 0, in: clo)
}

// CHECK-LABEL: @AD__{{.*}}closureCaptureMutable{{.*}}___vjp_src_0_wrt_0
// CHECK: bb0({{%.*}} : $Float, [[BOXED_ARG:%.*]] : ${ var Float }):
// CHECK:   [[PRIMAL:%.*]] = function_ref @AD__{{.*}}closureCaptureMutable{{.*}}___primal_src_0_wrt_0
// CHECK:   {{.*}} = apply [[PRIMAL]]({{.*}}, [[BOXED_ARG]])
// CHECK:   [[ADJOINT:%.*]] = function_ref @AD__{{.*}}closureCaptureMutabley{{.*}}___adjoint_src_0_wrt_0
// CHECK:   {{.*}} = partial_apply [callee_guaranteed] [[ADJOINT]]({{.*}}, {{.*}}, {{.*}}, [[BOXED_ARG]])
