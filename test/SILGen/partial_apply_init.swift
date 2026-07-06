// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [ossa] @$s18partial_apply_init06class_c1_a1_B0{{[_0-9a-zA-Z]*}}F
func class_init_partial_apply(c: C.Type) {
  // Partial applications at the static metatype should use a direct reference.
  // CHECK: function_ref @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESicfu_ : $@convention(thin) (Int) -> @owned C
  let xC: (Int) -> C = C.init
  // CHECK: function_ref @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESdcfu0_
  let requiredC: (Double) -> C = C.init

  // Partial applications to a dynamic metatype must be dynamically dispatched,
  // CHECK: function_ref @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESdcfu1_
  let requiredM: (Double) -> C = c.init
}

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESicfu_ : $@convention(thin) (Int) -> @owned C
// CHECK:         function_ref @$s18partial_apply_init1CC1xACSi_tcfC

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESdcfu0_ : $@convention(thin) (Double) -> @owned C
// CHECK:         function_ref @$s18partial_apply_init1CC8requiredACSd_tcfC

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init06class_c1_a1_B01cyAA1CCm_tFAESdcfu1_ : $@convention(thin) (Double, @thick C.Type) -> @owned C
// CHECK:         class_method %1 : $@thick C.Type, #C.init!allocator

// CHECK-LABEL: sil hidden [ossa] @$s18partial_apply_init010archetype_c1_a1_B0{{[_0-9a-zA-Z]*}}F
func archetype_init_partial_apply<T: C>(t: T.Type) where T: P {
  // Archetype initializations are always dynamic, whether applied to the type or a metatype.
  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSdcfu_
  let requiredT: (Double) -> T = T.init
  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSScfu0_
  let protoT: (String) -> T = T.init
  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSfcfu1_
  let protoExtT: (Float) -> T = T.init

  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSdcfu2_
  let requiredM: (Double) -> T = t.init
  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSScfu3_
  let protoM: (String) -> T = t.init
  // CHECK: function_ref @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSfcfu4_
  let protoExtM: (Float) -> T = t.init
}

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSdcfu_ :
// CHECK:         class_method %1 : $@thick C.Type, #C.init!allocator : (C.Type) -> (Double) -> C, $@convention(method) (Double, @thick C.Type) -> @owned C

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSScfu0_ :
// CHECK:         witness_method $T, #P.init!allocator : <Self where Self : P> (Self.Type) -> (String) -> Self

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSfcfu1_ :
// CHECK:         function_ref @$s18partial_apply_init1PPAAE8protoExtxSf_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : P> (Float, @thick τ_0_0.Type) -> @out τ_0_0

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSdcfu2_ :
// CHECK:         class_method %1 : $@thick C.Type, #C.init!allocator : (C.Type) -> (Double) -> C

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSScfu3_ :
// CHECK:         witness_method $T, #P.init!allocator : <Self where Self : P> (Self.Type) -> (String) -> Self

// CHECK-LABEL: sil private [ossa] @$s18partial_apply_init010archetype_c1_a1_B01tyxm_tAA1CCRbzAA1PRzlFxSfcfu4_ :
// CHECK:         function_ref @$s18partial_apply_init1PPAAE8protoExtxSf_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : P> (Float, @thick τ_0_0.Type) -> @out τ_0_0
