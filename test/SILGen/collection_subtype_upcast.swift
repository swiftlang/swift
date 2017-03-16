// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @_T025collection_subtype_upcast06array_C0SayypGSayAA1SVG0D0_tF :
// CHECK:    bb0([[ARG:%.*]] : $Array<S>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_T0s15_arrayForceCastSayq_GSayxGr0_lF
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<S, Any>([[ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RESULT]]
func array_upcast(array: [S]) -> [Any] {
  return array
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
// CHECK-LABEL:      sil hidden @_T025collection_subtype_upcast05dict_C0s10DictionaryVyAA1SVypGADyAFSiG0D0_tF :
// CHECK:    bb0([[ARG:%.*]] : $Dictionary<S, Int>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_T0s17_dictionaryUpCasts10DictionaryVyq0_q1_GACyxq_Gs8HashableRzsAFR0_r2_lF
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<S, Int, S, Any>([[ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RESULT]]
func dict_upcast(dict: [S: Int]) -> [S: Any] {
  return dict
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
