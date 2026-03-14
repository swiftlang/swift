// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

public protocol P1 {}

public protocol P2 {
  associatedtype A: P1
}

public protocol P3 {
  associatedtype B: P2
  associatedtype A where B.A == A
}

public struct G<A: P1>: P2 {}

public func callee<T: P1>(_: T.Type) {}

// CHECK: rdar83687967.(file).caller11@
// CHECK: Generic signature: <Child where Child : P3, Child.[P3]B == G<Child.[P3]A>>
public func caller11<Child: P3>(_: Child)
    where Child.B == G<Child.A> {
  callee(Child.A.self)
}

// CHECK: rdar83687967.(file).caller12@
// CHECK: Generic signature: <Child where Child : P3, Child.[P3]B == G<Child.[P3]A>>
public func caller12<Child: P3>(_: Child)
    where Child.B == G<Child.A>, Child.A : P1 {

  // Make sure IRGen can evaluate the conformance access path
  // (Child : P3)(Self.B : P2)(Self.A : P1).
  callee(Child.A.self)
}

// CHECK: rdar83687967.(file).X1@
// CHECK: Requirement signature: <Self where Self.[X1]Child : P3, Self.[X1]Child.[P3]B == G<Self.[X1]Child.[P3]A>>
public protocol X1 {
  associatedtype Child: P3
    where Child.B == G<Child.A>
}

// CHECK: rdar83687967.(file).X2@
// CHECK: Requirement signature: <Self where Self.[X2]Child : P3, Self.[X2]Child.[P3]B == G<Self.[X2]Child.[P3]A>>

public protocol X2 {
  associatedtype Child: P3
    where Child.B == G<Child.A>, Child.A : P1
}

public func caller21<T : X1>(_: T) {
  // Make sure IRGen can evaluate the conformance access path
  // (T : X1)(Child : P3)(Self.B : P2)(Self.A : P1).
  callee(T.Child.A.self)
}

public func caller22<T : X2>(_: T) {
  // Make sure IRGen can evaluate the conformance access path
  // (T : X2)(Child : P3)(Self.B : P2)(Self.A : P1).
  callee(T.Child.A.self)
}
