// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P {
  associatedtype T where T == Self
}

protocol Q : P {}

extension P {
  func missing() {}
}

extension P where T : Q {
  // CHECK-LABEL: Generic signature: <Self where Self : Q>
  func test() {
    missing()
  }
}

class C : P {}

extension P where T : C {
  // CHECK-LABEL: Generic signature: <Self where Self == C>
  func test() {
    missing()
  }
}

struct S : P {}

extension P where T == S {
  // CHECK-LABEL: Generic signature: <Self where Self == S>
  func test() {
    missing()
  }
}