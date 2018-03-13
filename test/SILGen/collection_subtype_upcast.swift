// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -module-name collection_subtype_upcast -emit-silgen -enable-sil-ownership -sdk %S/Inputs %s | %FileCheck %s

struct S { var x, y: Int }

// CHECK-LABEL: sil hidden @$S25collection_subtype_upcast06array_C00D0SayypGSayAA1SVG_tF :
// CHECK:    bb0([[ARG:%.*]] : @owned $Array<S>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$Ss15_arrayForceCastySayq_GSayxGr0_lF
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
// CHECK-LABEL:      sil hidden @$S25collection_subtype_upcast05dict_C00D0s10DictionaryVyAA1SVypGAEyAGSiG_tF :
// CHECK:    bb0([[ARG:%.*]] : @owned $Dictionary<S, Int>):
// CHECK-NEXT: debug_value [[ARG]]
// CHECK-NEXT: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK-NEXT: [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$Ss17_dictionaryUpCastys10DictionaryVyq0_q1_GACyxq_Gs8HashableRzsAFR0_r2_lF
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]<S, Int, S, Any>([[ARG_COPY]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK-NEXT: end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK-NEXT: destroy_value [[ARG]]
// CHECK-NEXT: return [[RESULT]]
func dict_upcast(dict: [S: Int]) -> [S: Any] {
  return dict
}

// It's not actually possible to test this for Sets independent of
// the bridging rules.
