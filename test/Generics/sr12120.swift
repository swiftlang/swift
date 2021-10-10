// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr12120.(file).Swappable1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Swapped.Swapped, Self.A == Self.Swapped.B, Self.Swapped : Swappable1>
protocol Swappable1 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable1
    where Swapped.B == A,
          Swapped.A == B, // FIXME: Diagnose redundancy
          Swapped.Swapped == Self
}

// CHECK: sr12120.(file).Swappable2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Swapped.Swapped, Self.A == Self.Swapped.B, Self.Swapped : Swappable2>
protocol Swappable2 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable2
    where Swapped.B == A,
          Swapped.Swapped == Self
}

// CHECK: sr12120.(file).Swappable3@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Swapped.Swapped, Self.B == Self.Swapped.A, Self.Swapped : Swappable3>
protocol Swappable3 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable3
    where Swapped.A == B,
          Swapped.Swapped == Self
}
