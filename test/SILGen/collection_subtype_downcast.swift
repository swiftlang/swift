// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @_T027collection_subtype_downcast06array_C0SayAA1SVGSgSayypG0D0_tF :
// CHECK:    bb0([[ARG:%.*]] : $Array<Any>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_T0s21_arrayConditionalCastSayq_GSgSayxGr0_lF
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Any, S>([[ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@owned Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
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
// CHECK-LABEL:      sil hidden @_T027collection_subtype_downcast05dict_C0s10DictionaryVyAA1SVSiGSgADyAFypG0D0_tF :
// CHECK:    bb0([[ARG:%.*]] : $Dictionary<S, Any>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @_T0s30_dictionaryDownCastConditionals10DictionaryVyq0_q1_GSgACyxq_Gs8HashableRzsAGR0_r2_lF
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<S, Any, S, Int>([[ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RESULT]]
func dict_downcast(dict: [S: Any]) -> [S: Int]? {
  return dict as? [S: Int]
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
