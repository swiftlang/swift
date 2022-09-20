// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

// rdar://95992916 - SILGen crash due to incorrect overload pick for `==`

enum E : String {
case a
case b
}

protocol P : AnyObject {
  var prop: E { get }
}

func test(arr: [any P]) {
  _ = arr.map {
    let v = (a: $0.prop, b: $0.prop)
    switch v {
    case let (a, b) where a == b: break
    default: break
    }
  }
}

// CHECK: sil private [ossa] @$s34anyhashable_and_operator_filtering4test3arrySayAA1P_pG_tFyAaD_pXEfU_
// CHECK: [[LHS_ARG:%.*]] = alloc_stack $E
// CHECK: [[RHS_ARG:%.*]] = alloc_stack $E
// CHECK: function_ref == infix<A>(_:_:)
// CHECK-NEXT: [[GENERIC_OP:%.*]] = function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF : $@convention(thin) <τ_0_0 where τ_0_0 : RawRepresentable, τ_0_0.RawValue : Equatable> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> Bool
// CHECK-NEXT: apply [[GENERIC_OP]]<E>([[LHS_ARG]], [[RHS_ARG]])
