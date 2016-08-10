// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Associated {
  associatedtype Assoc
}

struct Abstracted<T: Associated, U: Associated> {
  let closure: (T.Assoc) -> U.Assoc
}

struct S1 {}
struct S2 {}

// CHECK-LABEL: sil hidden @_TF21same_type_abstraction28callClosureWithConcreteTypes
// CHECK:         function_ref @_TTR
func callClosureWithConcreteTypes<
  T: Associated, U: Associated
  where
  T.Assoc == S1, U.Assoc == S2
>(x x: Abstracted<T, U>, arg: S1) -> S2 {
  return x.closure(arg)
}
