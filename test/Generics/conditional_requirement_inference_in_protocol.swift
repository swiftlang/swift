// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Good@
// CHECK-LABEL: Requirement signature: <Self where Self.T == Array<Self.U>, Self.U : Equatable>

protocol Good {
  associatedtype T : Equatable // expected-warning {{redundant conformance constraint 'Self.T' : 'Equatable'}}
  associatedtype U : Equatable where T == Array<U> // expected-note {{conformance constraint 'Self.T' : 'Equatable' implied here}}
}

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Bad@
// CHECK-LABEL: Requirement signature: <Self where Self.T == Array<Self.U>>

protocol Bad {
  associatedtype T : Equatable // expected-warning {{redundant conformance constraint 'Self.T' : 'Equatable'}}
  associatedtype U where T == Array<U> // expected-note {{conformance constraint 'Self.T' : 'Equatable' implied here}}
}
