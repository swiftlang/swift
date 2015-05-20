// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Associated {
  typealias Assoc
}

struct Abstracted<T: Associated, U: Associated> {
  let closure: T.Assoc -> U.Assoc
}

struct S1 {}
struct S2 {}

// CHECK-LABEL: sil hidden @_TF21same_type_abstraction28callClosureWithConcreteTypesu0_Rq_S_10Associatedq0_S0_zqq_S0_5AssocVS_2S1zqq0_S0_5AssocVS_2S2_FT1xGVS_10Abstractedq_q0__3argS1__S2_
// CHECK:         function_ref @_TTRG0_Rq_21same_type_abstraction10Associatedq0_S0_zqq_S0_5AssocVS_2S1zqq0_S0_5AssocVS_2S2_XFo_iS1__iS2__XFo_dS1__dS2__ 
func callClosureWithConcreteTypes<
  T: Associated, U: Associated
  where
  T.Assoc == S1, U.Assoc == S2
>(x x: Abstracted<T, U>, arg: S1) -> S2 {
  return x.closure(arg)
}
