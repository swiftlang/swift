// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s -check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s -check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s -check-prefix=CHECK-SIL

//===----------------------------------------------------------------------===//
// Closure conversion
//===----------------------------------------------------------------------===//

func thin(x: Float) -> Float { return x }

func myfunction(_ f: @escaping @differentiable (Float) -> (Float)) -> (Float) -> Float {
  // @differentiable functions should be callable.
  _ = f(.zero)
  return f
}

func apply() {
  _ = myfunction(thin)
}

// CHECK-AST-LABEL:  (func_decl {{.*}} "myfunction(_:)"
// CHECK-AST:          (call_expr type='(Float)'
// CHECK-AST:            (declref_expr type='@differentiable (Float) -> (Float)'
// CHECK-AST:          (return_stmt
// CHECK-AST:            (function_conversion_expr implicit type='(Float) -> Float'
// CHECK-AST:              (autodiff_function_extract_original implicit type='(Float) -> (Float)'
// CHECK-AST:                (declref_expr type='@differentiable (Float) -> (Float)'
// CHECK-AST-LABEL:  (func_decl {{.*}} "apply()"
// CHECK-AST:          (function_conversion_expr implicit type='@differentiable (Float) -> (Float)'
// CHECK-AST:            (autodiff_function implicit type='@differentiable (Float) -> Float'
// CHECK-AST:              (declref_expr type='(Float) -> Float'

// CHECK-SILGEN-LABEL: @{{.*}}myfunction{{.*}}
// CHECK-SILGEN: bb0([[DIFFED:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[ORIG:%.*]] = copy_value [[DIFFED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_ORIG:%.*]] = begin_borrow [[ORIG]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   apply [[BORROWED_ORIG]]({{%.*}}) : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   destroy_value [[ORIG]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[DIFFED_COPY:%.*]] = copy_value [[DIFFED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG:%.*]] = autodiff_function_extract [original] [[DIFFED_COPY]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   return [[ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-SILGEN-LABEL: @{{.*}}apply{{.*}}
// CHECK-SILGEN:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SILGEN-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN-NEXT:  [[DIFFED:%.*]] = autodiff_function [wrt 0] [order 1] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float

// CHECK-SIL:  [[DIFFED:%.*]] = autodiff_function [wrt 0] [order 1] {{%.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-SIL:  release_value [[DIFFED]] : $@differentiable @callee_guaranteed (Float) -> Float

//===----------------------------------------------------------------------===//
// Reabstraction
//===----------------------------------------------------------------------===//

func pullback<T, R>(
  at x: T, in f: @escaping @differentiable (T) -> R
) -> (R.TangentVector) -> T.TangentVector {
  fatalError()
}

func appliesReabstraction(_ f: @escaping @differentiable (Float) -> Float) {
  _ = pullback(at: .zero, in: f)
}

// CHECK-SILGEN-LABEL: sil hidden [ossa] @{{.*}}appliesReabstraction{{.*}}
// CHECK-SILGEN: bb0([[DIFF_FUNC_ARG:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[DIFF_FUNC:%.*]] = copy_value [[DIFF_FUNC_ARG]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[DIFF_FUNC_BORROWED:%.*]] = begin_borrow [[DIFF_FUNC]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG:%.*]] = autodiff_function_extract [original] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG_COPY:%.*]] = copy_value [[ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[REABS_ORIG:%.*]] = function_ref @$sS2fIegyd_S2fIegnr_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> Float) -> @out Float
// CHECK-SILGEN:   [[NEW_ORIG:%.*]] = partial_apply [callee_guaranteed] [[REABS_ORIG]]([[ORIG_COPY]]) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> Float) -> @out Float
// CHECK-SILGEN:   [[JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[JVP_COPY:%.*]] = copy_value [[JVP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[JVP_THUNK:%.*]] = function_ref @$sS4fIegnr_Iegydo_S4fIegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[NEW_JVP:%.*]] = partial_apply [callee_guaranteed] [[JVP_THUNK]]([[JVP_COPY]])
// CHECK-SILGEN:   [[VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[VJP_COPY:%.*]] = copy_value [[VJP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[VJP_THUNK:%.*]] = function_ref @$sS4fIegnr_Iegydo_S4fIegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[NEW_VJP:%.*]] = partial_apply [callee_guaranteed] [[VJP_THUNK]]([[VJP_COPY]])
// CHECK-SILGEN:   [[NEW_DIFF_FUNC:%.*]] = autodiff_function [wrt 0] [order 1] [[NEW_ORIG]] : $@callee_guaranteed (@in_guaranteed Float) -> @out Float with {[[NEW_JVP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float), [[NEW_VJP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)}
// CHECK-SILGEN:   [[DIFF_API:%.*]] = function_ref @${{.*}}pullback{{.*}}at{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Differentiable, τ_0_1 : Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_1) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1.TangentVector) -> @out τ_0_0.TangentVector
// CHECK-SILGEN:   apply [[DIFF_API]]<Float, Float>({{.*}}, [[NEW_DIFF_FUNC]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Differentiable, τ_0_1 : Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_1) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1.TangentVector) -> @out τ_0_0.TangentVector
