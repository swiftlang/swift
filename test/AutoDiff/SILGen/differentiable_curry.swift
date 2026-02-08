// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

import _Differentiation

func foo(_ x: Float, _ y: () -> Float) -> Float {
  if x > 0 {
    return y()
  }
  return x
}

@differentiable(reverse)
func bar(_ x: Float, _ y: Float) -> Float {
  return foo(x, {y})
}

// CHECK-LABEL: {{^}}// reverse-mode derivative of bar(_:_:)
// CHECK:       sil hidden @$s20differentiable_curry3baryS2f_SftFTJrSSpSr : $@convention(thin) (Float, Float) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
// CHECK:       bb0(%0 : $Float, %1 : $Float):
// CHECK:         // function_ref closure #1 in bar(_:_:)
// CHECK:         %[[#T4:]] = function_ref @$s20differentiable_curry3baryS2f_SftFSfyXEfU_ : $@convention(thin) (Float) -> Float
// CHECK:         %[[#T5:]] = differentiability_witness_function [jvp] [reverse] [parameters 0] [results 0] @$s20differentiable_curry3baryS2f_SftFSfyXEfU_ : $@convention(thin) (Float) -> Float
// CHECK:         %[[#T6:]] = differentiability_witness_function [vjp] [reverse] [parameters 0] [results 0] @$s20differentiable_curry3baryS2f_SftFSfyXEfU_ : $@convention(thin) (Float) -> Float
// CHECK:         %[[#T7:]] = differentiable_function [parameters 0] [results 0] %[[#T4]] : $@convention(thin) (Float) -> Float with_derivative {%[[#T5]] : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float), %[[#T6]] : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)}
// CHECK:         %[[#T8:]] = differentiable_function_extract [vjp] %[[#T7]] : $@differentiable(reverse) @convention(thin) (Float) -> Float
// CHECK:         %[[#T9:]] = partial_apply [callee_guaranteed] [on_stack] %[[#T8]](%1) : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK:         // function_ref foo(_:_:)
// CHECK:         %[[#T10:]] = function_ref @$s20differentiable_curry3fooyS2f_SfyXEtF : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> Float) -> Float
// CHECK:         %[[#T11:]] = differentiability_witness_function [jvp] [reverse] [parameters 0 1] [results 0] @$s20differentiable_curry3fooyS2f_SfyXEtF : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> Float) -> Float
// CHECK:         %[[#T12:]] = differentiability_witness_function [vjp] [reverse] [parameters 0 1] [results 0] @$s20differentiable_curry3fooyS2f_SfyXEtF : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> Float) -> Float
// CHECK:         %[[#T13:]] = differentiable_function [parameters 0 1] [results 0] %[[#T10]] : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> Float) -> Float with_derivative {%[[#T11]] : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (Float, @owned @callee_guaranteed (Float, @guaranteed Float) -> Float), %[[#T12]] : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK:         %[[#T14:]] = differentiable_function_extract [vjp] %[[#T13]] : $@differentiable(reverse) @convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> Float) -> Float
// CHECK:         %[[#T15:]] = apply %[[#T14]](%0, %[[#T9]]) : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))
// ...

// CHECK-LABEL: {{^}}// reverse-mode derivative of foo(_:_:)
// CHECK:       sil private @$s20differentiable_curry3fooyS2f_SfyXEtFTJrSSpSr : $@convention(thin) (Float, @guaranteed @noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
// CHECK:       bb0(%0 : $Float, %1 : $@noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)):
// CHECK:         cond_br %[[#]], bb1, bb2
// CHECK:       bb1:
// CHECK:         %[[#T9:]] = enum $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, #_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1.bb0!enumelt, %[[#]] : $()
// CHECK:         %[[#T10:]] = apply %1() : $@noescape @callee_guaranteed () -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK:         %[[#T11:]] = tuple_extract %[[#T10]] : $(Float, @callee_guaranteed (Float) -> Float), 0
// CHECK:         %[[#T12:]] = tuple_extract %[[#T10]] : $(Float, @callee_guaranteed (Float) -> Float), 1
// CHECK:         %[[#T13:]] = tuple $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, @callee_guaranteed (Float) -> Float) (%[[#T9]], %[[#T12]])
// CHECK:         %[[#T14:]] = enum $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1, #_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1.bb1!enumelt, %[[#T13]] : $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, @callee_guaranteed (Float) -> Float)
// CHECK:         br bb3(%[[#]] : $Float, %[[#]] : $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1)
// CHECK:       bb2:
// ...
// CHECK:       bb3(%[[#]] : $Float, %[[#]] : $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1):
// ...

// CHECK-LABEL: {{^}}// pullback of foo(_:_:)
// CHECK:       sil private @$s20differentiable_curry3fooyS2f_SfyXEtFTJpSSpSr : $@convention(thin) (Float, @owned _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1) -> (Float, Float) {
// CHECK:       bb0(%0 : $Float, %1 : $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1):
// CHECK:         switch_enum %1 : $_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1, case #_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1.bb2!enumelt: bb1, case #_AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb3__Pred__src_0_wrt_0_1.bb1!enumelt: bb2
// CHECK:       bb1(%[[#]] : $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb2__Pred__src_0_wrt_0_1)):
// ...
// CHECK:       bb2(%[[#T15:]] : $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, @callee_guaranteed (Float) -> Float)):
// CHECK:         %[[#T16:]] = tuple_extract %[[#T15]] : $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, @callee_guaranteed (Float) -> Float), 0
// CHECK:         %[[#T17:]] = tuple_extract %[[#T15]] : $(predecessor: _AD__$s20differentiable_curry3fooyS2f_SfyXEtF_bb1__Pred__src_0_wrt_0_1, @callee_guaranteed (Float) -> Float), 1
// CHECK:         %[[#T18:]] = apply %[[#T17]](%0) : $@callee_guaranteed (Float) -> Float
// CHECK:         strong_release %[[#T17]] : $@callee_guaranteed (Float) -> Float
// CHECK:         %[[#T20:]] = struct_extract %[[#T18]] : $Float, #Float._value
// ...
