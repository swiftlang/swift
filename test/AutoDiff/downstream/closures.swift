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
// CHECK:   differentiable_function_extract [original] [[CLOSURE]] : $@differentiable @callee_guaranteed (Float) -> Float

struct InoutAliasableCapture {
  var x: Float = .zero
  mutating func foo() {
    func capturesMutableSelf(t: Float) -> Float {
      self.x = .zero
      return t
    }
    _ = gradient(at: .zero, in: capturesMutableSelf)
  }
}

// CHECK-LABEL: @{{.*}}InoutAliasableCapture{{.*}}foo{{.*}} : $@convention(method) (@inout InoutAliasableCapture) -> () {
// CHECK: bb0([[SELF:%.*]] : $*InoutAliasableCapture):
// CHECK:   [[JVP:%.*]] = differentiability_witness_function [jvp] [parameters 0] [results 0] @{{.*}}capturesMutableSelf{{.*}} : $@convention(thin) (Float, @inout_aliasable InoutAliasableCapture) -> Float
// CHECK-NOT:  retain_value_addr [[SELF]]
// CHECK-NOT:  copy_addr [[SELF]]
// CHECK:   [[JVP_CAPTURED:%.*]] = partial_apply [callee_guaranteed] [[JVP]]([[SELF]]) : $@convention(thin) (Float, @inout_aliasable InoutAliasableCapture) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK:   [[VJP:%.*]] = differentiability_witness_function [vjp] [parameters 0] [results 0] @{{.*}}capturesMutableSelf{{.*}} : $@convention(thin) (Float, @inout_aliasable InoutAliasableCapture) -> Float
// CHECK-NOT:  retain_value_addr [[SELF]]
// CHECK-NOT:  retain_value_addr [[SELF]]
// CHECK-NOT:  copy_addr [[SELF]]
// CHECK:   [[VJP_CAPTURED:%.*]] = partial_apply [callee_guaranteed] [[VJP]]([[SELF]]) : $@convention(thin) (Float, @inout_aliasable InoutAliasableCapture) -> (Float, @owned @callee_guaranteed (Float) -> Float)

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

// Make sure `@noDerivative` gets propagated through SIL.
// Make sure `@noDerivative` with non-`Differentiable` also works.
public func nondiffs(_ f: @differentiable (Float, @noDerivative Float) -> Float,
                     _ g: @differentiable (Float, @noDerivative Int) -> Float) {
  _ = gradient(at: 0) { f($0, 1) }
  _ = gradient(at: 0) { g($0, 1) }
}
nondiffs({ x, y in x }, { x, y in x })

// Crasher when SILGen'ing @differentiable functions with generic @noDerivative parameters.
func foo<T>(_ f: @differentiable (Float, @noDerivative T) -> Float, _ t: T) -> Float {
  return f(1, t)
}
