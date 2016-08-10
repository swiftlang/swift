// RUN: %target-swift-frontend -enable-experimental-collection-casts -emit-silgen -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @_TF27collection_subtype_downcast14array_downcastFT5arrayGSaP___GSqGSaVS_1S__ :
// CHECK:    bb0(%0 : $Array<Any>):
// CHECK-NEXT: debug_value %0
// CHECK-NEXT: retain_value %0
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_TFs21_arrayConditionalCastu0_rFGSax_GSqGSaq___
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Any, S>(%0) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
// CHECK-NEXT: release_value %0
// CHECK-NEXT: return [[RESULT]]
func array_downcast(array: [Any]) -> [S]? {
  return array as? [S]
}

extension S : Hashable {
  var hashValue : Int {
    return x + y
  }
}
func ==(lhs: S, rhs: S) -> Bool {
  return true
}

// FIXME: This entrypoint name should not be bridging-specific
// CHECK-LABEL:      sil hidden @_TF27collection_subtype_downcast13dict_downcastFT4dictGVs10DictionaryVS_1SP___GSqGS0_S1_Si__ :
// CHECK:    bb0(%0 : $Dictionary<S, Any>):
// CHECK-NEXT: debug_value %0
// CHECK-NEXT: retain_value %0
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_TFs30_dictionaryDownCastConditionalu2_Rxs8Hashable0_S_rFGVs10Dictionaryxq__GSqGS0_q0_q1___
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<S, Any, S, Int>(%0) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
// CHECK-NEXT: release_value %0
// CHECK-NEXT: return [[RESULT]]
func dict_downcast(dict: [S: Any]) -> [S: Int]? {
  return dict as? [S: Int]
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
