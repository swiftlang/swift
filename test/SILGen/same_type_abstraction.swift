// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Associated {
  typealias Assoc
}

struct Abstracted<T: Associated, U: Associated> {
  let closure: T.Assoc -> U.Assoc
}

struct S1 {}
struct S2 {}

// CHECK-LABEL: sil hidden @_TF21same_type_abstraction28callClosureWithConcreteTypesUS_10Associated_S0___FT1xGVS_10AbstractedQ_Q0__3argVS_2S1_VS_2S2
// CHECK:         function_ref @_TTRG1_RPq_P21same_type_abstraction10Associated_Pq0_PS0__Eqq_5AssocVS_2S1Eqq0_5AssocVS_2S2_XFo_iS1__iS2__XFo_dS1__dS2__ 
func callClosureWithConcreteTypes<
  T: Associated, U: Associated
  where
  T.Assoc == S1, U.Assoc == S2
>(#x: Abstracted<T, U>, #arg: S1) -> S2 {
  return x.closure(arg)
}
