// RUN: %target-typecheck-verify-swift -requirement-machine-inferred-signatures=verify
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=verify -requirement-machine-inferred-signatures=verify 2>&1 | %FileCheck %s

class C {}

struct S {}

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T == C>
protocol P1 {
  associatedtype T where T : C, T == C
  // expected-warning@-1 {{redundant superclass constraint 'Self.T' : 'C'}}
  // expected-note@-2 {{superclass constraint 'Self.T' : 'C' implied here}}
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self>
protocol P2 {
  associatedtype T where T : C, T == S
  // expected-error@-1 {{'Self.T' requires that 'S' inherit from 'C'}}
  // expected-note@-2 {{superclass constraint 'Self.T' : 'C' implied here}}
  // expected-note@-3 {{same-type constraint 'Self.T' == 'S' implied here}}
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self>
protocol P3 {
  associatedtype T where T == S, T : C
  // expected-error@-1 {{'Self.T' requires that 'S' inherit from 'C'}}
  // expected-note@-2 {{same-type constraint 'Self.T' == 'S' implied here}}
  // expected-note@-3 {{superclass constraint 'Self.T' : 'C' implied here}}
}

protocol P4a {
  associatedtype T : C
}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]T : P4>
protocol P4 {
  associatedtype T : P4 where T.T == S
  // expected-error@-1 2{{same-type constraint type 'S' does not conform to required protocol 'P4'}}
}

class D {}

// CHECK-LABEL: .P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]T == D>
protocol P5 {
  associatedtype T where T : C, T == D
  // expected-error@-1 {{'Self.T' requires that 'D' inherit from 'C'}}
  // expected-note@-2 {{superclass constraint 'Self.T' : 'C' implied here}}
  // expected-note@-3 {{same-type constraint 'Self.T' == 'D' implied here}}
}
