// RUN: %target-swift-frontend -emit-silgen -enable-experimental-differentiable-programming %s | %FileCheck %s
// REQUIRES: differentiable_programming

// Test SILGen for `@differentiable` function typed values.

import _Differentiation

@_silgen_name("differentiable")
func differentiable(_ fn: @escaping @differentiable (Float) -> Float)
    -> @differentiable (Float) -> Float {
  return fn
}

@_silgen_name("linear")
func linear(_ fn: @escaping @differentiable(linear) (Float) -> Float)
    -> @differentiable(linear) (Float) -> Float {
  return fn
}

@_silgen_name("differentiable_noDerivative")
func differentiable_noDerivative(
  _ fn: @escaping @differentiable (Float, @noDerivative Float) -> Float
) -> @differentiable (Float, @noDerivative Float) -> Float {
  return fn
}

@_silgen_name("linear_noDerivative")
func linear_noDerivative(
  _ fn: @escaping @differentiable(linear) (Float, @noDerivative Float) -> Float
) -> @differentiable(linear) (Float, @noDerivative Float) -> Float {
  return fn
}

// CHECK-LABEL: sil hidden [ossa] @differentiable : $@convention(thin) (@guaranteed @differentiable @callee_guaranteed (Float) -> Float) -> @owned @differentiable @callee_guaranteed (Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @linear : $@convention(thin) (@guaranteed @differentiable(linear) @callee_guaranteed (Float) -> Float) -> @owned @differentiable(linear) @callee_guaranteed (Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(linear) @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @differentiable_noDerivative : $@convention(thin) (@guaranteed @differentiable @callee_guaranteed (Float, @noDerivative Float) -> Float) -> @owned @differentiable @callee_guaranteed (Float, @noDerivative Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float, @noDerivative Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @linear_noDerivative : $@convention(thin) (@guaranteed @differentiable(linear) @callee_guaranteed (Float, @noDerivative Float) -> Float) -> @owned @differentiable(linear) @callee_guaranteed (Float, @noDerivative Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(linear) @callee_guaranteed (Float, @noDerivative Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(linear) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(linear) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK: }
