// RUN: %target-typecheck-verify-swift -debug-generic-signatures -disable-requirement-machine-reuse 2>&1 | %FileCheck %s
// RUN: %target-typecheck-verify-swift -dump-requirement-machine -disable-requirement-machine-reuse 2>&1 | %FileCheck %s --check-prefix=CHECK-RULE

// Note: The GSB fails this test, because it doesn't implement unification of
// superclass type constructor arguments.

// FIXME: The Requirement Machine also fails to minimize the signature of
// unifySuperclassTest(). rdar://90469643

class Generic<T, U, V> {}

protocol P1 {
  associatedtype X : Generic<Int, A1, B1>
  associatedtype A1
  associatedtype B1
}

protocol P2 {
  associatedtype X : Generic<A2, String, B2>
  associatedtype A2
  associatedtype B2
}

func sameType<T>(_: T.Type, _: T.Type) {}

func takesGenericIntString<U>(_: Generic<Int, String, U>.Type) {}

// CHECK-LABEL: .unifySuperclassTest@
// CHECK-NEXT: Generic signature: <T where T : P1, T : P2, T.[P1]A1 == String>

// FIXME: The 'T.[P1]A1 == String' requirement is redundant and should not appear
// in the minimal signature.

func unifySuperclassTest<T : P1 & P2>(_: T) {
  sameType(T.A1.self, String.self)
  sameType(T.A2.self, Int.self)
  sameType(T.B1.self, T.B2.self)
  takesGenericIntString(T.X.self)
}

// CHECK-RULE-LABEL: Requirement machine for <τ_0_0 where τ_0_0 : P1, τ_0_0 : P2, τ_0_0.A1 == String>
// CHECK-RULE-NEXT: Rewrite system: {
// CHECK-RULE:      - τ_0_0.[P1:A1].[concrete: String] => τ_0_0.[P1:A1]
// CHECK-RULE:      - τ_0_0.[P2:X] => τ_0_0.[P1:X]
// CHECK-RULE:      - τ_0_0.[P1:X].[superclass: Generic<τ_0_0.[P2:A2], String, τ_0_0.[P2:B2]>] => τ_0_0.[P1:X]
// CHECK-RULE:      - τ_0_0.[P1:X].[superclass: Generic<Int, String, τ_0_0.[P1:B1]>] => τ_0_0.[P1:X]
// CHECK-RULE:      - τ_0_0.[P2:A2].[concrete: Int] => τ_0_0.[P2:A2]
// CHECK-RULE:      - τ_0_0.[P2:B2] => τ_0_0.[P1:B1]
// CHECK-RULE:      - τ_0_0.B2 => τ_0_0.[P1:B1]
// CHECK-RULE:      }
// CHECK-RULE: Property map: {
// CHECK-RULE-NEXT:   [P1] => { conforms_to: [P1 Copyable Escapable] }
// CHECK-RULE-NEXT:   [P1:A1] => { conforms_to: [Copyable Escapable] }
// CHECK-RULE-NEXT:   [P1:B1] => { conforms_to: [Copyable Escapable] }
// CHECK-RULE-NEXT:   [P1:X] => { layout: _NativeClass superclass: [superclass: Generic<Int, [P1:A1], [P1:B1]>] }
// CHECK-RULE-NEXT:   [P2] => { conforms_to: [P2 Copyable Escapable] }
// CHECK-RULE-NEXT:   [P2:A2] => { conforms_to: [Copyable Escapable] }
// CHECK-RULE-NEXT:   [P2:B2] => { conforms_to: [Copyable Escapable] }
// CHECK-RULE-NEXT:   [P2:X] => { layout: _NativeClass superclass: [superclass: Generic<[P2:A2], String, [P2:B2]>] }
// CHECK-RULE-NEXT:   [Copyable] => { conforms_to: [Copyable] }
// CHECK-RULE-NEXT:   [Escapable] => { conforms_to: [Escapable] }
// CHECK-RULE-NEXT:   τ_0_0 => { conforms_to: [P1 P2 Copyable Escapable] }
// CHECK-RULE-NEXT:   τ_0_0.[P1:A1] => { conforms_to: [Copyable Escapable] concrete_type: [concrete: String] }
// CHECK-RULE-NEXT:   τ_0_0.[P1:X] => { layout: _NativeClass superclass: [superclass: Generic<Int, String, τ_0_0.[P1:B1]>] }
// CHECK-RULE-NEXT:   τ_0_0.[P2:A2] => { conforms_to: [Copyable Escapable] concrete_type: [concrete: Int] }
// CHECK-RULE-NEXT: }
