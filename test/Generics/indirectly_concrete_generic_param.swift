// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

class S<T, U> where T : P, U == T.T {}

protocol P {
  associatedtype T
}

struct G<X, T, U> {}

class C : P {
  typealias T = Int
}

// CHECK-LABEL: Generic signature: <X, T, U where X : S<T, C.T>, T : C, U == C.T>
extension G where X : S<T, U>, T : C {}
