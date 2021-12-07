// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK-LABEL: rdar77683844.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.T1 : P2, Self.T1 == Self.T3.T1, Self.T2 == Self.T1.T2, Self.T3 == Self.T1.T3, Self.T1.T2 == Self.T1.T4.T2, Self.T1.T3 == Self.T2.T3>

public protocol P1 {
    associatedtype T1: P2 where T1.T4.T2 == T2, T1.T2 == T2, T1.T3 == T3
    associatedtype T2 where T2.T3 == T3
    associatedtype T3 where T3.T1 == T1
}

// CHECK-LABEL: rdar77683844.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.T2 : P4, Self.T3 : P3, Self.T4 : P5>

public protocol P2 {
    associatedtype T3: P3
    associatedtype T2: P4
    associatedtype T4: P5
}

// CHECK-LABEL: rdar77683844.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.T1 : P2, Self.T2 : P4>

public protocol P3 {
    associatedtype T1: P2
    associatedtype T2: P4
}

// CHECK-LABEL: rdar77683844.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self.T3 : P3>

public protocol P4 {
    associatedtype T3: P3
}

// CHECK-LABEL: rdar77683844.(file).P5@
// CHECK-LABEL: Requirement signature: <Self where Self.T2 : P4>

public protocol P5 {
    associatedtype T2: P4
}