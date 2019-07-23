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
  _ = gradient(at: 0) { (x: Float) -> Float in
    val += 2
    return val * x
  }
}

// CHECK-LABEL: @AD__{{.*}}closureCaptureMutable{{.*}}___vjp_src_0_wrt_0
// CHECK: bb0({{%.*}} : $Float, [[INOUT_ARG:%.*]] : ${ var Float }):
// CHECK:   [[ADJOINT:%.*]] = function_ref @AD__{{.*}}closureCaptureMutabley{{.*}}___pullback_src_0_wrt_0
// CHECK:   {{.*}} = partial_apply [callee_guaranteed] [[ADJOINT]]({{.*}})

// TF-30: VJP return value should match the return type.
struct TF_30 : Differentiable {
  var x: Float
  @noDerivative var y: @differentiable (Float) -> Float
}
// Make sure this passes SIL verification.
let _: @differentiable (TF_30) -> Float = { s in s.x }

// Make sure `@nondiff` gets propagated through SIL.
// Make sure `@nondiff` with non-`Differentiable` also works.
public func nondiffs(_ f: @differentiable (Float, @nondiff Float) -> Float,
                     _ g: @differentiable (Float, @nondiff Int) -> Float) {
  _ = gradient(at: 0) { f($0, 1) }
  _ = gradient(at: 0) { g($0, 1) }
}
nondiffs({ x, y in x }, { x, y in x })

// Crasher when SILGen'ing @differentiable functions with generic @nondiff parameters.
func foo<T>(_ f: @differentiable (Float, @nondiff T) -> Float, _ t: T) -> Float {
  return f(1, t)
}
