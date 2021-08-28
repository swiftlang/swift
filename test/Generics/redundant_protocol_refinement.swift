// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol Base {}
protocol Middle : Base {}
protocol Derived : Middle, Base {}
// expected-note@-1 {{conformance constraint 'Self' : 'Base' implied here}}
// expected-warning@-2 {{redundant conformance constraint 'Self' : 'Base'}}

protocol P1 {}

protocol P2 {
  associatedtype Assoc: P1
}

// no warning here
protocol Good: P2, P1 where Assoc == Self {}
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self : P2, Self == Self.Assoc>

// missing refinement of 'P1'
protocol Bad: P2 where Assoc == Self {}
// expected-warning@-1 {{protocol 'Bad' should be declared to refine 'P1' due to a same-type constraint on 'Self'}}
// expected-note@-2 {{conformance constraint 'Self' : 'P1' implied here}}
// CHECK-LABEL: Requirement signature: <Self where Self : P2, Self == Self.Assoc>