// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s -check-prefix=CHECK-AST
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

func myfunction2(_ f: @escaping @differentiable(linear) (Float) -> (Float)) /*-> (Float) -> Float*/ {
  // @differentiable(linear) functions should be callable.
  _ = f(.zero)
  // TODO(TF-900): Uncomment the following line to test conversion to non-differentiable function type.
  // return f
}

var global_f: @differentiable (Float) -> Float = {$0}
// TODO(TF-902): Uncomment the following line to test linear function storage.
// var global_f_linear: @differentiable(linear) (Float) -> Float = {$0}

func calls_global_f() {
  _ = global_f(10)
  // TODO(TF-900, TF-902): Uncomment the following line to test loading a linear function from memory and direct calls to a linear function.
  // _ = global_f_linear(10)
}

func apply() {
  _ = myfunction(thin)
  // TODO(TF-900): Uncomment the following line to test direct calls to a linear function.
  // _ = myfunction2(thin)
}

// CHECK-AST-LABEL:  (func_decl {{.*}} "myfunction(_:)"
// CHECK-AST:          (call_expr type='(Float)'
// CHECK-AST:            (declref_expr type='@differentiable (Float) -> (Float)'
// CHECK-AST:          (return_stmt
// CHECK-AST:            (function_conversion_expr implicit type='(Float) -> Float'
// CHECK-AST:              (differentiable_function_extract_original implicit type='(Float) -> (Float)'
// CHECK-AST:                (declref_expr type='@differentiable (Float) -> (Float)'
// CHECK-AST-LABEL:  (func_decl {{.*}} "apply()"
// CHECK-AST:          (function_conversion_expr implicit type='@differentiable (Float) -> (Float)'
// CHECK-AST:            (differentiable_function implicit type='@differentiable (Float) -> Float'
// CHECK-AST:              (declref_expr type='(Float) -> Float'

// CHECK-SILGEN-LABEL: @{{.*}}myfunction{{.*}}
// CHECK-SILGEN: bb0([[DIFF:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[COPIED_DIFF:%.*]] = copy_value [[DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_DIFF:%.*]] = begin_borrow [[COPIED_DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   apply [[BORROWED_DIFF]]({{%.*}}) : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   end_borrow [[BORROWED_DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   destroy_value [[COPIED_DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[COPIED_DIFF:%.*]] = copy_value [[DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_DIFF:%.*]] = begin_borrow [[COPIED_DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_ORIG:%.*]] = differentiable_function_extract [original] [[BORROWED_DIFF]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[COPIED_ORIG:%.*]] = copy_value [[BORROWED_ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   return [[COPIED_ORIG]] : $@callee_guaranteed (Float) -> Float

// CHEK-SILGEN-LABEL: @{{.*}}myfunction2{{.*}} : $@convention(thin) (@guaranteed @differentiable(linear) @callee_guaranteed (Float) -> Float) -> () {
// CHEK-SILGEN: bb0([[LIN:%.*]] : @guaranteed $@differentiable(linear) @callee_guaranteed (Float) -> Float):
// CHEK-SILGEN:   [[COPIED_LIN:%.*]] = copy_value [[LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHEK-SILGEN:   apply [[COPIED_LIN]]<Float>({{%.*}}, {{%.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AdditiveArithmetic, τ_0_0 : ExpressibleByIntegerLiteral> (@thick τ_0_0.Type) -> @out τ_0_0
// CHEK-SILGEN:   [[BORROWED_LIN:%.*]] = begin_borrow [[COPIED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHEK-SILGEN:   apply [[BORROWED_LIN]]({{%.*}}) : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHEK-SILGEN:   end_borrow [[BORROWED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHEK-SILGEN:   destroy_value [[COPIED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float // id: %13
// TODO(TF-900): Change this to returning the extracted original function.
// CHEK-SILGEN:   %14 = tuple ()
// CHEK-SILGEN:   return %14 : $()

// CHECK-SILGEN-LABEL: @{{.*}}apply{{.*}}
// CHECK-SILGEN:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SILGEN-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN-NEXT:  [[DIFFED:%.*]] = differentiable_function [wrt 0] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float

// CHECK-SIL:  [[DIFFED:%.*]] = differentiable_function [wrt 0] {{%.*}} : $@callee_guaranteed (Float) -> Float

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

// CHECK-SILGEN-LABEL: @{{.*}}appliesReabstraction{{.*}}
// CHECK-SILGEN: bb0([[DIFF_FUNC_ARG:%.*]] : @guaranteed $@differentiable @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[DIFF_FUNC:%.*]] = copy_value [[DIFF_FUNC_ARG]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[DIFF_FUNC_BORROWED:%.*]] = begin_borrow [[DIFF_FUNC]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG:%.*]] = differentiable_function_extract [original] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG_COPY:%.*]] = copy_value [[ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[REABS_ORIG:%.*]] = function_ref @$sS2fIegyd_S2fIegnr_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> Float) -> @out Float
// CHECK-SILGEN:   [[NEW_ORIG:%.*]] = partial_apply [callee_guaranteed] [[REABS_ORIG]]([[ORIG_COPY]]) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> Float) -> @out Float
// CHECK-SILGEN:   [[JVP:%.*]] = differentiable_function_extract [jvp] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[JVP_COPY:%.*]] = copy_value [[JVP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SILGEN:   [[REABS_JVP:%.*]] = function_ref @$sS4fIegyd_Iegydo_S4fIegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[NEW_JVP:%.*]] = partial_apply [callee_guaranteed] [[REABS_JVP]]([[JVP_COPY]]) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[VJP:%.*]] = differentiable_function_extract [vjp] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[VJP_COPY:%.*]] = copy_value [[VJP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SILGEN:   [[REABS_VJP:%.*]] = function_ref @$sS4fIegyd_Iegydo_S4fIegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[NEW_VJP:%.*]] = partial_apply [callee_guaranteed] [[REABS_VJP]]([[VJP_COPY]]) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SILGEN:   [[NEW_DIFF_FUNC:%.*]] = differentiable_function [wrt 0] [[NEW_ORIG]] : $@callee_guaranteed (@in_guaranteed Float) -> @out Float with {[[NEW_JVP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float), [[NEW_VJP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)}
// CHECK-SILGEN:   [[DIFF_API:%.*]] = function_ref @${{.*}}pullback{{.*}}at{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Differentiable, τ_0_1 : _Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_1) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1.TangentVector) -> @out τ_0_0.TangentVector
// HECK-SILGEN:   apply [[DIFF_API]]<Float, Float>({{.*}}, [[NEW_DIFF_FUNC]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : _Differentiable, τ_0_1 : _Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_1) -> @owned @callee_guaranteed (@in_guaranteed τ_0_1.TangentVector) -> @out τ_0_0.TangentVector
