// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

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
// expected-warning@-1 {{non-final class 'C' cannot safely conform to protocol 'P', which requires that 'Self.T' is exactly equal to 'Self'; this is an error in the Swift 6 language mode}}

extension P where T : C {
  // CHECK-LABEL: Generic signature: <Self where Self : C>
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
