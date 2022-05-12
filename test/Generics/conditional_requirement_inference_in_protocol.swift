// RUN: %target-typecheck-verify-swift -warn-redundant-requirements
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// FIXME(rqm-diagnostics): The redundant conformance warnings here should not be emitted, since
// these requirements participate in conditional requirement inference.

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Good@
// CHECK-LABEL: Requirement signature: <Self where Self.[Good]T == [Self.[Good]U], Self.[Good]U : Equatable>

protocol Good {
  associatedtype T : Equatable // expected-warning {{redundant conformance constraint 'Array<Self.U>' : 'Equatable'}}
  associatedtype U : Equatable where T == Array<U>
  // expected-warning@-1 {{redundant conformance constraint 'Self.U' : 'Equatable'}}
}

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Bad@
// CHECK-LABEL: Requirement signature: <Self where Self.[Bad]T == [Self.[Bad]U], Self.[Bad]U : Equatable>

protocol Bad {
  associatedtype T : Equatable // expected-warning {{redundant conformance constraint 'Array<Self.U>' : 'Equatable'}}
  associatedtype U where T == Array<U>
}
