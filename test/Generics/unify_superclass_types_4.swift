// RUN: %target-typecheck-verify-swift -requirement-machine=on -debug-requirement-machine 2>&1 | %FileCheck %s

// Note: The GSB fails this test, because it doesn't implement unification of
// superclass type constructor arguments.

class Base<T> {}

protocol Q {
  associatedtype T
}

class Derived<TT : Q> : Base<TT.T> {}

protocol P1 {
  associatedtype X : Base<A1>
  associatedtype A1
}

protocol P2 {
  associatedtype X : Derived<A2>
  associatedtype A2 : Q
}

func sameType<T>(_: T.Type, _: T.Type) {}

func takesBase<U>(_: Base<U>.Type, _: U.Type) {}

func takesDerived<U : Q>(_: Derived<U>.Type, _: U.Type) {}

func unifySuperclassTest<T : P1 & P2>(_: T) {
  sameType(T.A1.self, T.A2.T.self)
  takesBase(T.X.self, T.A1.self)
  takesDerived(T.X.self, T.A2.self)
}

// CHECK-LABEL: Requirement machine for <τ_0_0 where τ_0_0 : P1, τ_0_0 : P2>
// CHECK-NEXT: Rewrite system: {
// CHECK:      - τ_0_0.[P1&P2:X].[superclass: Derived<τ_0_0> with <τ_0_0.[P2:A2]>] => τ_0_0.[P1&P2:X]
// CHECK-NEXT: - τ_0_0.[P1&P2:X].[superclass: Base<τ_0_0> with <τ_0_0.[P1:A1]>] => τ_0_0.[P1&P2:X]
// CHECK-NEXT: - τ_0_0.[P1&P2:X].[layout: _NativeClass] => τ_0_0.[P1&P2:X]
// CHECK-NEXT: - τ_0_0.[P2:A2].[Q:T] => τ_0_0.[P1:A1]
// CHECK-NEXT: }
// CHECK-NEXT: Property map: {
// CHECK-NEXT:   [P1:X] => { layout: _NativeClass superclass: [superclass: Base<τ_0_0> with <[P1:A1]>] }
// CHECK-NEXT:   [P2:A2] => { conforms_to: [Q] }
// CHECK-NEXT:   [P2:X] => { layout: _NativeClass superclass: [superclass: Derived<τ_0_0> with <[P2:A2]>] }
// CHECK-NEXT:   τ_0_0 => { conforms_to: [P1 P2] }
// CHECK-NEXT:   τ_0_0.[P1&P2:X] => { layout: _NativeClass superclass: [superclass: Derived<τ_0_0> with <τ_0_0.[P2:A2]>] }
// CHECK-NEXT: }