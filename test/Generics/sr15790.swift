// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: sr15790.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]X : P1, Self.[P1]X == Self.[P1]X.[P1]X>
public protocol P1 {
  associatedtype X: P1 where X.X == X
}

// CHECK-LABEL: sr15790.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self : Collection, Self : P1, Self.[Sequence]Element : P1, Self.[P1]X == Self.[P2]Z.[P2]Y, Self.[P2]Y : P3, Self.[P2]Z : P2>
public protocol P2: Collection, P1 where Element: P1, X == Z.Y {
  associatedtype Y: P3
  associatedtype Z: P2
}

// CHECK-LABEL: sr15790.(file).P3@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P3]T : P2>
public protocol P3: P2 {
  associatedtype T: P2
}
