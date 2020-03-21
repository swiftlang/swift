// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s -check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s -check-prefix=CHECK-SILGEN

//===----------------------------------------------------------------------===//
// Closure conversion
//===----------------------------------------------------------------------===//

func thin(x: Float) -> Float { return x }

func myfunction(_ f: @escaping @differentiable (Float) -> (Float)) -> (Float) -> Float {
  // @differentiable functions should be callable.
  _ = f(.zero)
  return f
}

func myfunction2(_ f: @escaping @differentiable(linear) (Float) -> (Float)) -> (Float) -> Float {
  // @differentiable(linear) functions should be callable.
  _ = f(.zero)
  return f
}

var global_f: @differentiable (Float) -> Float = {$0}
var global_f_linear: @differentiable(linear) (Float) -> Float = {$0}

func calls_global_f() {
  _ = global_f(10)
  // TODO(TF-900, TF-902): Uncomment the following line to test loading a linear function from memory and direct calls to a linear function.
  // _ = global_f_linear(10)
}

func apply() {
  _ = myfunction(thin)
  _ = myfunction2(thin)
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

// CHECK-SILGEN-LABEL: @{{.*}}myfunction2{{.*}}
// CHECK-SILGEN: bb0([[LIN:%.*]] : @guaranteed $@differentiable(linear) @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[COPIED_LIN:%.*]] = copy_value [[LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_LIN:%.*]] = begin_borrow [[COPIED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   apply [[BORROWED_LIN]]({{%.*}}) : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   end_borrow [[BORROWED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[COPIED_LIN:%.*]] = copy_value [[LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_LIN:%.*]] = begin_borrow [[COPIED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_ORIG:%.*]] = linear_function_extract [original] [[BORROWED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[COPIED_ORIG:%.*]] = copy_value [[BORROWED_ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   end_borrow [[BORROWED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   destroy_value [[COPIED_LIN]] : $@differentiable(linear) @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   return [[COPIED_ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-SILGEN-LABEL: @{{.*}}apply{{.*}}
// CHECK-SILGEN:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SILGEN-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN-NEXT:  [[DIFFED:%.*]] = differentiable_function [parameters 0] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SILGEN-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN-NEXT:  [[LIN:%.*]] = linear_function [parameters 0] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float

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
// CHECK-SILGEN:   [[NEW_ORIG_CONVERTED:%.*]] = convert_function [[NEW_ORIG]] : $@callee_guaranteed (@in_guaranteed Float) -> @out Float to $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>
// CHECK-SILGEN:   [[JVP:%.*]] = differentiable_function_extract [jvp] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[JVP_COPY:%.*]] = copy_value [[JVP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SILGEN:   [[REABS_JVP:%.*]] = function_ref @$sS4fIegyd_Iegydo_S2fxq_r0_lyS2fIsegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>)
// CHECK-SILGEN:   [[NEW_JVP:%.*]] = partial_apply [callee_guaranteed] %19(%18) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>)
// CHECK-SILGEN:   [[NEW_JVP_CONVERTED:%.*]] = convert_function [[NEW_JVP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>) to $@callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_2, τ_0_3>) for <Float, Float, Float, Float>
// CHECK-SILGEN:   [[VJP:%.*]] = differentiable_function_extract [vjp] [[DIFF_FUNC_BORROWED]] : $@differentiable @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[VJP_COPY:%.*]] = copy_value [[VJP]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SILGEN:   [[REABS_VJP:%.*]] = function_ref @$sS4fIegyd_Iegydo_S2fxq_r0_lyS2fIsegnr_Iegnro_TR : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>)
// CHECK-SILGEN:   [[NEW_VJP:%.*]] = partial_apply [callee_guaranteed] [[REABS_VJP]]([[VJP_COPY]]) : $@convention(thin) (@in_guaranteed Float, @guaranteed @callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>)
// CHECK-SILGEN:   [[NEW_VJP_CONVERTED:%.*]] = convert_function [[NEW_VJP]] : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float>) to $@callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_2, τ_0_3>) for <Float, Float, Float, Float>
// CHECK-SILGEN:   [[NEW_DIFF_FUNC:%.*]] = differentiable_function [parameters 0] [[NEW_ORIG_CONVERTED]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Float, Float> with_derivative {[[NEW_JVP_CONVERTED]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_2, τ_0_3>) for <Float, Float, Float, Float>, [[NEW_VJP_CONVERTED]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2, τ_0_3> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_2, τ_0_3>) for <Float, Float, Float, Float>}
// CHECK-SILGEN:   [[DIFF_API:%.*]] = function_ref @${{.*}}pullback{{.*}}at{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Differentiable, τ_0_1 : Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_0, τ_0_1>) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_1.TangentVector, τ_0_0.TangentVector>
// CHECK-SILGEN:   apply [[DIFF_API]]<Float, Float>({{.*}}, [[NEW_DIFF_FUNC]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Differentiable, τ_0_1 : Differentiable> (@in_guaranteed τ_0_0, @guaranteed @differentiable @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_0, τ_0_1>) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <τ_0_1.TangentVector, τ_0_0.TangentVector>
