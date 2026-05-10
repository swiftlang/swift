// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Good@
// CHECK-LABEL: Requirement signature: <Self where Self.[Good]T == [Self.[Good]U], Self.[Good]U : Equatable>

protocol Good {
  associatedtype T : Equatable
  associatedtype U : Equatable where T == Array<U>
}

// CHECK-LABEL: conditional_requirement_inference_in_protocol.(file).Bad@
// CHECK-LABEL: Requirement signature: <Self where Self.[Bad]T == [Self.[Bad]U], Self.[Bad]U : Equatable>

protocol Bad {
  associatedtype T : Equatable
  associatedtype U where T == Array<U>
}
