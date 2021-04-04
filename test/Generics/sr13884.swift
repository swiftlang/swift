// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -emit-ir -debug-generic-signatures %s 2>&1 | %FileCheck %s

public protocol P {
  associatedtype A : Q where A.B == Self
}

public protocol Q {
  associatedtype B
}

public class C : P {
  public typealias A = D
}

public class D : Q {
  public typealias B = C
}

// Both <T : P & C> and <T : C & P> minimize to <T where T == C>:
// - T : P and T : C imply that T.A == C.A == D;
// - T : P also implies that T.A.B == T, via A.B == Self in P;
// - Since T.A == D, T.A.B == D.B, therefore Self == D.B.
// - D.B is a typealias for C, so really Self == C.

// CHECK-LABEL: Generic signature: <T where T == D.B>
public func takesBoth1<T : P & C>(_: T) {}
// expected-warning@-1 {{redundant conformance constraint 'T' : 'P'}}
// expected-note@-2 {{conformance constraint 'T' : 'P' implied here}}

// CHECK-LABEL: Generic signature: <U where U == D.B>
public func takesBoth2<U : C & P>(_: U) {}
// expected-warning@-1 {{redundant conformance constraint 'U' : 'P'}}
// expected-note@-2 {{conformance constraint 'U' : 'P' implied here}}
