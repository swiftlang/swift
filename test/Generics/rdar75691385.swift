// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P {}

struct S : P {}

struct G<T : P> {}

extension G {
  // CHECK-LABEL: Generic signature: <T, U where T == S, U : AnyObject>
  func f<U>(_: U) where T == S, U : AnyObject {}
}
