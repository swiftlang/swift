
// RUN: %target-swift-emit-silgen -module-name collection_subtype_downcast -enable-sil-ownership -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @$s27collection_subtype_downcast06array_C00D0SayAA1SVGSgSayypG_tF :
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $Array<Any>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$ss21_arrayConditionalCastySayq_GSgSayxGr0_lF
// CHECK-NEXT: [[BORROWED_ARG_COPY:%.*]] = begin_borrow [[ARG_COPY]]
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<Any, S>([[BORROWED_ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Optional<Array<τ_0_1>>
// CHECK-NEXT: end_borrow
// CHECK-NEXT: destroy_value [[ARG_COPY]]
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
// CHECK-LABEL:      sil hidden @$s27collection_subtype_downcast05dict_C00D0SDyAA1SVSiGSgSDyAEypG_tF :
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $Dictionary<S, Any>):
// CHECK: debug_value [[ARG]]
// CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK: // function_ref
// CHECK: [[FN:%.*]] = function_ref @$ss30_dictionaryDownCastConditionalySDyq0_q1_GSgSDyxq_GSHRzSHR0_r2_lF
// CHECK: [[BORROWED_ARG_COPY:%.*]] = begin_borrow [[ARG_COPY]]
// CHECK: [[RESULT:%.*]] = apply [[FN]]<S, Any, S, Int>([[BORROWED_ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Optional<Dictionary<τ_0_2, τ_0_3>>
// CHECK: end_borrow [[BORROWED_ARG_COPY]]
// CHECK: destroy_value [[ARG_COPY]]
// CHECK: return [[RESULT]]
func dict_downcast(dict: [S: Any]) -> [S: Int]? {
  return dict as? [S: Int]
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
