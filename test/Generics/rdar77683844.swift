// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: rdar77683844.(file).P1@
// CHECK-LABEL: Requirement signature: <Self where Self.[P1]T1 : P2, Self.[P1]T1 == Self.[P1]T3.[P3]T1, Self.[P1]T2 == Self.[P1]T1.[P2]T2, Self.[P1]T3 == Self.[P1]T1.[P2]T3, Self.[P1]T1.[P2]T2 == Self.[P1]T1.[P2]T4.[P5]T2, Self.[P1]T1.[P2]T3 == Self.[P1]T2.[P4]T3>

public protocol P1 {
    associatedtype T1: P2 where T1.T4.T2 == T2, T1.T2 == T2, T1.T3 == T3
    associatedtype T2 where T2.T3 == T3
    associatedtype T3 where T3.T1 == T1
}

// CHECK-LABEL: rdar77683844.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.[P2]T2 : P4, Self.[P2]T3 : P3, Self.[P2]T4 : P5>

public protocol P2 {
    associatedtype T3: P3
    associatedtype T2: P4
    associatedtype T4: P5
}

// CHECK-LABEL: rdar77683844.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self.[P3]T1 : P2, Self.[P3]T2 : P4>

public protocol P3 {
    associatedtype T1: P2
    associatedtype T2: P4
}

// CHECK-LABEL: rdar77683844.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self.[P4]T3 : P3>

public protocol P4 {
    associatedtype T3: P3
}

// CHECK-LABEL: rdar77683844.(file).P5@
// CHECK-LABEL: Requirement signature: <Self where Self.[P5]T2 : P4>

public protocol P5 {
    associatedtype T2: P4
}
