
// RUN: %target-swift-emit-silgen -module-name collection_subtype_upcast -enable-sil-ownership -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @$s25collection_subtype_upcast06array_C00D0SayypGSayAA1SVG_tF :
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $Array<S>):
// CHECK: debug_value [[ARG]]
// CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK: // function_ref
// CHECK: [[FN:%.*]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF
// CHECK: [[BORROWED_ARG_COPY:%.*]] = begin_borrow [[ARG_COPY]]
// CHECK: [[RESULT:%.*]] = apply [[FN]]<S, Any>([[BORROWED_ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: end_borrow [[BORROWED_ARG_COPY]]
// CHECK: destroy_value [[ARG_COPY]]
// CHECK: return [[RESULT]]
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
// CHECK-LABEL:      sil hidden @$s25collection_subtype_upcast05dict_C00D0SDyAA1SVypGSDyAESiG_tF :
// CHECK:    bb0([[ARG:%.*]] : @guaranteed $Dictionary<S, Int>):
// CHECK: debug_value [[ARG]]
// CHECK: [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK: // function_ref
// CHECK: [[FN:%.*]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF
// CHECK: [[BORROWED_ARG_COPY:%.*]] = begin_borrow [[ARG_COPY]]
// CHECK: [[RESULT:%.*]] = apply [[FN]]<S, Int, S, Any>([[BORROWED_ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: destroy_value [[ARG_COPY]]
// CHECK: return [[RESULT]]
func dict_upcast(dict: [S: Int]) -> [S: Any] {
  return dict
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
