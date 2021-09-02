// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

public protocol P1 {
  associatedtype A
}

public protocol P2 {}

public struct S1: P1 {
  public typealias A = S2
}

public struct S2: P2 {}

// CHECK-LABEL: Generic signature: <X, Y where X : P1, Y : P2, Y == X.A>
public struct G<X: P1, Y: P2> where Y == X.A {}

// CHECK-LABEL: Generic signature: <X, Y where X == S1, Y == S1.A>
public extension G where X == S1 {}
