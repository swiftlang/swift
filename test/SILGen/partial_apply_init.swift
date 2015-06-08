// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class C {
  init(x: Int) {}

  required init(required: Double) {}
}

class D {
  required init(required: Double) {}
}

protocol P {
  init(proto: String)
}

extension P {
  init(protoExt: Float) {
    self.init(proto: "")
  }
}

// CHECK-LABEL: sil hidden @_TF18partial_apply_init24class_init_partial_applyFMCS_1CT_
func class_init_partial_apply(c: C.Type) {
  // Partial applications at the static metatype use the direct (_TTd) thunk.
  // CHECK: function_ref @_TTdFC18partial_apply_init1CCFMS0_FT1xSi_S0_
  let xC: Int -> C = C.init
  // CHECK: function_ref @_TTdFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
  let requiredC: Double -> C = C.init

  // Partial applications to a dynamic metatype must be dynamically dispatched and use
  // the normal thunk.
  // CHECK: function_ref @_TFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
  let requiredM: Double -> C = c.init
}

// CHECK-LABEL: sil shared @_TTdFC18partial_apply_init1CCFMS0_FT1xSi_S0_
// CHECK:         function_ref @_TFC18partial_apply_init1CCfMS0_FT1xSi_S0_
// CHECK-LABEL: sil shared @_TTdFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
// CHECK:         function_ref @_TFC18partial_apply_init1CCfMS0_FT8requiredSd_S0_
// CHECK-LABEL: sil shared @_TFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
// CHECK:         class_method %0 : $@thick C.Type, #C.init!allocator.1

// CHECK-LABEL: sil hidden @_TF18partial_apply_init28archetype_init_partial_applyuRdq_CS_1Cq_S_1P_FMq_T_
func archetype_init_partial_apply<T: C where T: P>(t: T.Type) {
  // Archetype initializations are always dynamic, whether applied to the type or a metatype.
  // CHECK: function_ref @_TFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
  let requiredT: Double -> T = T.init
  // CHECK: function_ref @_TFP18partial_apply_init1PCuRq_S0__FMq_FT5protoSS_q_
  let protoT: String -> T = T.init
  // CHECK: function_ref @_TFeRq_18partial_apply_init1P_S_S0_CuRq_S0__FMq_FT8protoExtSf_q_
  let protoExtT: Float -> T = T.init

  // CHECK: function_ref @_TFC18partial_apply_init1CCFMS0_FT8requiredSd_S0_
  let requiredM: Double -> T = t.init
  // CHECK: function_ref @_TFP18partial_apply_init1PCuRq_S0__FMq_FT5protoSS_q_
  let protoM: String -> T = t.init
  // CHECK: function_ref @_TFeRq_18partial_apply_init1P_S_S0_CuRq_S0__FMq_FT8protoExtSf_q_
  let protoExtM: Float -> T = t.init
}

// CHECK-LABEL: sil shared @_TFP18partial_apply_init1PCuRq_S0__FMq_FT5protoSS_q_
// CHECK:         witness_method $Self, #P.init!allocator.1
// CHECK-LABEL: sil shared @_TFeRq_18partial_apply_init1P_S_S0_CuRq_S0__FMq_FT8protoExtSf_q_
// CHECK:         function_ref @_TFeRq_18partial_apply_init1P_S_S0_CuRq_S0__fMq_FT8protoExtSf_q_

