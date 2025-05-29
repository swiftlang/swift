
// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt -sil-print-types | %FileCheck %s

protocol P {}

func bar(_: some P) {}

func foo(_ x: some P) {
  bar(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s26opaque_parameter_roundtrip3fooyyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> () {
// CHECK: bb0(%0 : $*τ_0_0):
// CHECK-NEXT: debug_value %0 : $*τ_0_0, let, name "x", argno 1, expr op_deref
// CHECK-NEXT: // function_ref bar<A>(_:)
// CHECK-NEXT: [[FN:%.*]] = function_ref @$s26opaque_parameter_roundtrip3baryyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: apply [[FN]]<τ_0_0>(%0) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()
// CHECK-NEXT: } // end sil function '$s26opaque_parameter_roundtrip3fooyyxAA1PRzlF'
