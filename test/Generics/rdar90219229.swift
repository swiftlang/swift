// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: .P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]T : C>
protocol P {
  typealias A = C

  associatedtype T : A
}

class C {}

protocol PBad {
  typealias A = C

  associatedtype B : P

  associatedtype T1 : B.A
  // expected-error@-1 {{type 'Self.T1' constrained to non-protocol, non-class type 'Self.B.A'}}

  associatedtype T2 where T2 : A
  // expected-error@-1 {{type 'Self.T2' constrained to non-protocol, non-class type 'Self.A'}}
}

// FIXME: Terrible diagnostics.

protocol PWorse {
// expected-error@-1 5{{circular reference}}
// expected-note@-2 9{{through reference here}}
  typealias A = C

  associatedtype T : Self.A
// expected-note@-1 5{{while resolving type 'Self.A'}}
// expected-note@-2 5{{through reference here}}
}
