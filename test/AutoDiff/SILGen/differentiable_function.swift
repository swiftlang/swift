// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// Test SILGen for `@differentiable` function typed values.

import _Differentiation

//===----------------------------------------------------------------------===//
// Return `@differentiable` function typed values unmodified.
//===----------------------------------------------------------------------===//

@_silgen_name("differentiable")
func differentiable(_ fn: @escaping @differentiable(reverse) (Float) -> Float)
    -> @differentiable(reverse) (Float) -> Float {
  return fn
}

@_silgen_name("linear")
func linear(_ fn: @escaping @differentiable(_linear) (Float) -> Float)
    -> @differentiable(_linear) (Float) -> Float {
  return fn
}

@_silgen_name("differentiable_noDerivative")
func differentiable_noDerivative(
  _ fn: @escaping @differentiable(reverse) (Float, @noDerivative Float) -> Float
) -> @differentiable(reverse) (Float, @noDerivative Float) -> Float {
  return fn
}

@_silgen_name("linear_noDerivative")
func linear_noDerivative(
  _ fn: @escaping @differentiable(_linear) (Float, @noDerivative Float) -> Float
) -> @differentiable(_linear) (Float, @noDerivative Float) -> Float {
  return fn
}

// CHECK-LABEL: sil hidden [ossa] @differentiable : $@convention(thin) (@guaranteed @differentiable(reverse) @callee_guaranteed (Float) -> Float) -> @owned @differentiable(reverse) @callee_guaranteed (Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(reverse) @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @linear : $@convention(thin) (@guaranteed @differentiable(_linear) @callee_guaranteed (Float) -> Float) -> @owned @differentiable(_linear) @callee_guaranteed (Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(_linear) @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @differentiable_noDerivative : $@convention(thin) (@guaranteed @differentiable(reverse) @callee_guaranteed (Float, @noDerivative Float) -> Float) -> @owned @differentiable(reverse) @callee_guaranteed (Float, @noDerivative Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(reverse) @callee_guaranteed (Float, @noDerivative Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(reverse) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(reverse) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK: }

// CHECK-LABEL: sil hidden [ossa] @linear_noDerivative : $@convention(thin) (@guaranteed @differentiable(_linear) @callee_guaranteed (Float, @noDerivative Float) -> Float) -> @owned @differentiable(_linear) @callee_guaranteed (Float, @noDerivative Float) -> Float {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@differentiable(_linear) @callee_guaranteed (Float, @noDerivative Float) -> Float):
// CHECK:   [[COPIED_FN:%.*]] = copy_value [[FN]] : $@differentiable(_linear) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK:   return [[COPIED_FN]] : $@differentiable(_linear) @callee_guaranteed (Float, @noDerivative Float) -> Float
// CHECK: }

//===----------------------------------------------------------------------===//
// Closure conversion
//===----------------------------------------------------------------------===//

func thin(x: Float) -> Float { return x }

func myfunction(_ f: @escaping @differentiable(reverse) (Float) -> (Float)) -> (Float) -> Float {
  // @differentiable(reverse) functions should be callable.
  _ = f(.zero)
  return f
}

func myfunction2(_ f: @escaping @differentiable(_linear) (Float) -> (Float)) -> (Float) -> Float {
  // @differentiable(_linear) functions should be callable.
  _ = f(.zero)
  return f
}

var global_f: @differentiable(reverse) (Float) -> Float = {$0}
var global_f_linear: @differentiable(_linear) (Float) -> Float = {$0}

func calls_global_f() {
  _ = global_f(10)
  // TODO(TF-900, TF-902): Uncomment the following line to test loading a linear function from memory and direct calls to a linear function.
  // _ = global_f_linear(10)
}

func apply() {
  _ = myfunction(thin)
  _ = myfunction2(thin)
}

// CHECK-LABEL: @{{.*}}myfunction{{.*}}
// CHECK: bb0([[DIFF:%.*]] : @guaranteed $@differentiable(reverse) @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_DIFF:%.*]] = copy_value [[DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_DIFF:%.*]] = begin_borrow [[COPIED_DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   apply [[BORROWED_DIFF]]({{%.*}}) : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   end_borrow [[BORROWED_DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   destroy_value [[COPIED_DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   [[COPIED_DIFF:%.*]] = copy_value [[DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_DIFF:%.*]] = begin_borrow [[COPIED_DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_ORIG:%.*]] = differentiable_function_extract [original] [[BORROWED_DIFF]] : $@differentiable(reverse) @callee_guaranteed (Float) -> Float
// CHECK:   [[COPIED_ORIG:%.*]] = copy_value [[BORROWED_ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-LABEL: @{{.*}}myfunction2{{.*}}
// CHECK: bb0([[LIN:%.*]] : @guaranteed $@differentiable(_linear) @callee_guaranteed (Float) -> Float):
// CHECK:   [[COPIED_LIN:%.*]] = copy_value [[LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_LIN:%.*]] = begin_borrow [[COPIED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   apply [[BORROWED_LIN]]({{%.*}}) : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   end_borrow [[BORROWED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   [[COPIED_LIN:%.*]] = copy_value [[LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_LIN:%.*]] = begin_borrow [[COPIED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   [[BORROWED_ORIG:%.*]] = linear_function_extract [original] [[BORROWED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   [[COPIED_ORIG:%.*]] = copy_value [[BORROWED_ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK:   end_borrow [[BORROWED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   destroy_value [[COPIED_LIN]] : $@differentiable(_linear) @callee_guaranteed (Float) -> Float
// CHECK:   return [[COPIED_ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-LABEL: @{{.*}}apply{{.*}}
// CHECK:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-NEXT:  [[DIFFED:%.*]] = differentiable_function [parameters 0] [results 0] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float
// CHECK:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-NEXT:  [[LIN:%.*]] = linear_function [parameters 0] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float

