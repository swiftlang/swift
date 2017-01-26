// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

protocol Associated {
  associatedtype Assoc
}

struct Abstracted<T: Associated, U: Associated> {
  let closure: (T.Assoc) -> U.Assoc
}

struct S1 {}
struct S2 {}

// CHECK-LABEL: sil hidden @_T021same_type_abstraction28callClosureWithConcreteTypes{{[_0-9a-zA-Z]*}}F
// CHECK:         function_ref @_T0{{.*}}TR :
func callClosureWithConcreteTypes<
  T: Associated, U: Associated
  where
  T.Assoc == S1, U.Assoc == S2
>(x x: Abstracted<T, U>, arg: S1) -> S2 {
  return x.closure(arg)
}
