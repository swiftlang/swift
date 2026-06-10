// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

import _Differentiation

// Nothing special for throwing functions
@differentiable(reverse)
func throwing(input: Float) throws -> Float {
    return input
}

@differentiable(reverse)
// CHECK-LABEL: s5throw8catching5inputS2f_tFTJrSpSr
func catching(input: Float) -> Float {
  do {
    // Call VJP and wrap the pullback result into Optional
    // CHECK:   try_apply {{.*}} : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float, @error any Error), normal [[BB_NORM:.*]], error [[BB_ERROR:.*]] //
    // CHECK: [[BB_NORM]](%[[VWP:.*]] : $(Float, @callee_guaranteed (Float) -> Float))
    // CHECK:   %[[VALUE:.*]] = tuple_extract %[[VWP]] : $(Float, @callee_guaranteed (Float) -> Float), 0
    // CHECK:   %[[CALLEE_PULLBACK:.*]] = tuple_extract %[[VWP]] : $(Float, @callee_guaranteed (Float) -> Float), 1
    // CHECK:   %[[OPT_PULLBACK:.*]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.some!enumelt, %[[CALLEE_PULLBACK]] : $@callee_guaranteed (Float) -> Float
    // CHECK:   %[[NORMAL_PAYLOAD:.*]] = tuple (%[[OPT_PULLBACK]] : $Optional<@callee_guaranteed (Float) -> Float>)
    // CHECK:   enum $_AD__$s5throw8catching5inputS2f_tF_bb1__Pred__src_0_wrt_0, #_AD__$s5throw8catching5inputS2f_tF_bb1__Pred__src_0_wrt_0.bb0!enumelt, %[[NORMAL_PAYLOAD]] : $(_: Optional<@callee_guaranteed (Float) -> Float>)
    // CHECK:   br [[BB_RET:.*]](%[[VALUE]] : $Float
    // CHECK: [[BB_ERROR]](%[[ERROR:.*]] : $any Error):
    // CHECK:   %[[OPT_NOPULLBACK:.*]] = enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.none!enumelt
    // CHECK:   %[[ERROR_PAYLOAD:.*]] = tuple (%[[OPT_NOPULLBACK]] : $Optional<@callee_guaranteed (Float) -> Float>)
    // CHECK:   enum $_AD__$s5throw8catching5inputS2f_tF_bb3__Pred__src_0_wrt_0, #_AD__$s5throw8catching5inputS2f_tF_bb3__Pred__src_0_wrt_0.bb0!enumelt, %[[ERROR_PAYLOAD]] : $(_: Optional<@callee_guaranteed (Float) -> Float>)
    // CHECK:   br [[BB_RET]]
    // CHECK: [[BB_RET]](%[[RETVAL:[0-9]*]] : $Float,
    // CHECK:   %[[PULLBACK_REF:[0-9]+]] = function_ref @$s5throw8catching5inputS2f_tFTJpSpSr
    // CHECK:   %[[PULLBACK_CLOSURE:[0-9]+]] = partial_apply [callee_guaranteed] %[[PULLBACK_REF]](%{{.*}}) : $@convention(thin)
    // CHECK:   %[[RET_VWP:[0-9]+]] = tuple (%[[RETVAL]] : $Float, %[[PULLBACK_CLOSURE]] : $@callee_guaranteed (Float) -> Float)
    // CHECK:   return %[[RET_VWP]] : $(Float, @callee_guaranteed (Float) -> Float)
    return try throwing(input: input)
  } catch {
    return 0.0
  }
}

// See if pullback correctly unwraps the Optional
// CHECK-LABEL: sil {{.*}} @$s5throw8catching5inputS2f_tFTJpSpSr
// CHECK: bb{{.*}}(%[[ADJ:.*]] : $Float, %[[PAYLOAD:.*]] : $(_: Optional<@callee_guaranteed (Float) -> Float>)):
// CHECK:   %[[OPT_PULLBACK:.*]] = tuple_extract %[[PAYLOAD]] : $(_: Optional<@callee_guaranteed (Float) -> Float>), 0
// CHECK:   switch_enum %[[OPT_PULLBACK]] : $Optional<@callee_guaranteed (Float) -> Float>, case #Optional.some!enumelt: [[BB_NORMAL:.*]], case #Optional.none!enumelt: [[BB_ERROR:.*]] //
// CHECK: [[BB_NORMAL]](%[[PULLBACK:.*]] : $@callee_guaranteed (Float) -> Float)
// Call pullback and accumulate adjoints
// CHECK:   %[[CALLEE_ADJ:.*]] = apply %[[PULLBACK]](%{{.*}}) : $@callee_guaranteed (Float) -> Float
// CHECK-DAG:   %[[FLOAT_ADJ:.*]] = struct_extract %[[ADJ]] : $Float, #Float._value
// CHECK-DAG:   %[[FLOAT_CALLEE_ADJ:.*]] = struct_extract %[[CALLEE_ADJ]] : $Float, #Float._value
// CHECK:   %[[NEW_FLOAT_ADJ:.*]] = builtin "fadd_FPIEEE32"(%[[FLOAT_CALLEE_ADJ]] : $Builtin.FPIEEE32, %[[FLOAT_ADJ]] : $Builtin.FPIEEE32) : $Builtin.FPIEEE32
// CHECK:   %[[NEW_ADJ:.*]] = struct $Float (%[[NEW_FLOAT_ADJ]] : $Builtin.FPIEEE32)
// CHECK:   br {{.*}}(%[[NEW_ADJ]] : $Float)
// CHECK: [[BB_ERROR]]:
// Just forward input adjoint w/o any accumulation
// CHECK:   br bb{{.}}(%[[ADJ]] : $Float)
