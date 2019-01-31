// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

struct Foo {
  var x: Float
  var f: @differentiable (Float) -> Float
}
func diffableClosureInStruct(s: Foo) {
  _ = gradient(of: s.f)
}

// CHECK-LABEL: @{{.*}}diffableClosureInStruct{{.*}} : $@convention(thin) (@guaranteed Foo) -> () {
// CHECK:   [[CLOSURE:%.*]] = struct_extract {{%.*}} : $Foo, #Foo.f
// CHECK:   retain_value [[CLOSURE]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK:   autodiff_function_extract [original] [[CLOSURE]] : $@differentiable @callee_guaranteed (Float) -> Float


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
// CHECK:   {{.*}} = partial_apply [callee_guaranteed] [[ADJOINT]]({{.*}})

