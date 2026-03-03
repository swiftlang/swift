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

// Make sure that we don't pick a concrete `(AnyHashable, AnyHashable) -> Bool` overload.

// CHECK: sil private [ossa] @$s34anyhashable_and_operator_filtering4test3arrySayAA1P_pG_tFyAaD_pXEfU_
// CHECK: [[LHS_ARG:%.*]] = alloc_stack $E
// CHECK: [[RHS_ARG:%.*]] = alloc_stack $E
// CHECK: [[GENERIC_OP:%.*]] = witness_method $E, #Equatable."==" : <Self where Self : Equatable, Self : ~Copyable, Self : ~Escapable> (Self.Type) -> (borrowing Self, borrowing Self) -> Bool
// CHECK-NEXT: apply [[GENERIC_OP]]<E>([[LHS_ARG]], [[RHS_ARG]], {{.*}})
