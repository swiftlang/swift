// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt | %FileCheck %s

// Test SIL differentiability witness SIL generation.

// Test public non-generic function.
// SIL differentiability witness:
// - Has public linkage (implicit).
// - Has no `where` clause.

public func foo(_ x: Float) -> Float { x }

@differentiating(foo)
public func foo_jvp(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
  (x, { $0 })
}

@differentiating(foo)
public func foo_vjp(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { $0 })
}

// CHECK-LABEL: // differentiability witness for foo(_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3fooyS2fF : $@convention(thin) (Float) -> Float {
// CHECK-NEXT:   jvp: @AD__$s36sil_differentiability_witness_silgen3fooyS2fF__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-NEXT:   vjp: @AD__$s36sil_differentiability_witness_silgen3fooyS2fF__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-NEXT: }

// Test internal generic function.
// SIL differentiability witness:
// - Has hidden linkage.
// - Has `where` clause.

@differentiable(where T: Differentiable)
func generic<T>(_ x: T, _ y: Float) -> T { x }

@differentiating(generic)
func generic_jvp<T: Differentiable>(_ x: T, _ y: Float) -> (
  value: T, differential: (T.TangentVector, Float) -> T.TangentVector
) {
  (x, { dx, dy in dx })
}

@differentiating(generic)
func generic_vjp<T: Differentiable>(_ x: T, _ y: Float) -> (
  value: T, pullback: (T.TangentVector) -> (T.TangentVector, Float)
) {
  (x, { ($0, .zero) })
}

// CHECK-LABEL: // differentiability witness for generic<A>(_:_:)
// CHECK-NEXT: sil_differentiability_witness hidden [serialized] [parameters 0 1] [results 0] [where T : _Differentiable] @$s36sil_differentiability_witness_silgen7genericyxx_SftlF : $@convention(thin) <T> (@in_guaranteed T, Float) -> @out T {
// CHECK-NEXT:   jvp: @AD__$s36sil_differentiability_witness_silgen7genericyxx_SftlF__jvp_src_0_wrt_0_1 : $@convention(thin) <τ_0_0 where τ_0_0 : _Differentiable> (@in_guaranteed τ_0_0, Float) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector, Float) -> @out τ_0_0.TangentVector)
// CHECK-NEXT:   vjp: @AD__$s36sil_differentiability_witness_silgen7genericyxx_SftlF__vjp_src_0_wrt_0_1 : $@convention(thin) <τ_0_0 where τ_0_0 : _Differentiable> (@in_guaranteed τ_0_0, Float) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector) -> (@out τ_0_0.TangentVector, Float))
// CHECK-NEXT: }
