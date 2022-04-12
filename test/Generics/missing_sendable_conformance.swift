// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// REQUIRES: concurrency

// rdar://91174106 - allow missing Sendable conformances when extending a
// type generically.
// FIXME: Should warn because of missing Sendable, but currently is silent
// because we aren't checking conformance availability here yet.
class C {}

struct G1<T: Sendable> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G1
// CHECK-NEXT: Generic signature: <T where T == C>
extension G1 where T == C {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G1
// CHECK-NEXT: Generic signature: <T where T : C, T : Sendable>
extension G1 where T : C {}

protocol P {
  associatedtype A
}

struct G2<T : P> where T.A : Sendable {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G2
// CHECK-NEXT: Generic signature: <T where T : P, T.[P]A == C>
extension G2 where T.A == C {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=G2
// CHECK-NEXT: Generic signature: <T where T : P, T.[P]A : C, T.[P]A : Sendable>
extension G2 where T.A : C {}
