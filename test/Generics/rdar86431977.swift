// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A
  associatedtype B : P1 where B.A == A, B.B == B
}

protocol P2 : P1 where A == Self {}

struct G<T, U> {}

// The GSB used to get the signature of bar() wrong.

extension G {
  // CHECK-LABEL: rdar86431977.(file).G extension.foo()@
  // CHECK: Generic signature: <T, U where T : P2, T == U>
  func foo() where T : P2, U == T {}

  // CHECK-LABEL: rdar86431977.(file).G extension.bar()@
  // CHECK: Generic signature: <T, U where T : P2, T == U>
  func bar() where T : P2, T == U {}
}
