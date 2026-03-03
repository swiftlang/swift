// RUN: %target-swift-emit-silgen -disable-availability-checking %s | %FileCheck %s

protocol P<A> {
    associatedtype A
    associatedtype B
    var function: () -> B { get }
}

func f<A>(p: any P<A>) {
    _ = p.function()
}

// CHECK-LABEL: sil hidden [ossa] @$s40parameterized_existentials_reabstraction1f1pyAA1P_px1ARts_XP_tlF : $@convention(thin) <A> (@in_guaranteed any P<A>) -> () {

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s1B40parameterized_existentials_reabstraction1PPQyd__Iegr_ypIegr_1AQyd__RszAbCRd__r__lTR : $@convention(thin) <τ_0_0><τ_1_0 where τ_0_0 == τ_1_0.A, τ_1_0 : P> (@guaranteed @callee_guaranteed () -> @out τ_1_0.B) -> @out Any {
