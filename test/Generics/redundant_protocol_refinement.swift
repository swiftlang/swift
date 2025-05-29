// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

// CHECK-LABEL: redundant_protocol_refinement.(file).Base@
// CHECK-LABEL: Requirement signature: <Self>
protocol Base {}

// CHECK-LABEL: redundant_protocol_refinement.(file).Middle@
// CHECK-LABEL: Requirement signature: <Self where Self : Base>
protocol Middle : Base {}

// CHECK-LABEL: redundant_protocol_refinement.(file).Derived@
// CHECK-LABEL: Requirement signature: <Self where Self : Middle>
protocol Derived : Middle, Base {}

// CHECK-LABEL: redundant_protocol_refinement.(file).Derived2@
// CHECK-LABEL: Requirement signature: <Self where Self : Middle>
protocol Derived2 : Middle {}

// CHECK-LABEL: redundant_protocol_refinement.(file).MoreDerived@
// CHECK-LABEL: Requirement signature: <Self where Self : Derived2>
protocol MoreDerived : Derived2, Base {}

protocol P1 {}

protocol P2 {
  associatedtype Assoc: P1
}

// no warning here
protocol Good: P2, P1 where Assoc == Self {}

// CHECK-LABEL: redundant_protocol_refinement.(file).Good@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self : P2, Self == Self.[P2]Assoc>

// missing refinement of 'P1'
protocol Bad: P2 where Assoc == Self {}
// expected-warning@-1 {{protocol 'Bad' should be declared to refine 'P1' due to a same-type constraint on 'Self'}}

// CHECK-LABEL: redundant_protocol_refinement.(file).Bad@
// CHECK-LABEL: Requirement signature: <Self where Self : P2, Self == Self.[P2]Assoc>
