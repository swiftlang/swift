// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: rdar78233378.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self : P4, Self == Self.T1.T2, Self.T1 : P2>
public protocol P1: P4 where T1.T2 == Self {
    associatedtype T1: P2
}

// CHECK-LABEL: rdar78233378.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self : P5, Self.T2 : P1>
public protocol P2: P5 where T2: P1 {}

// CHECK-LABEL: rdar78233378.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.T2 : P4>
public protocol P3 {
    associatedtype T2: P4
}

// CHECK-LABEL: rdar78233378.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.T3.T2, Self.T3 : P3>
public protocol P4 where T3.T2 == Self {
    associatedtype T3: P3
}

// CHECK-LABEL: rdar78233378.(file).P5@
// CHECK-LABEL: Requirement signature: <Self where Self.T2 : P4>
public protocol P5 {
    associatedtype T2: P4
}