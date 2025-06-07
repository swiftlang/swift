// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/54555

// CHECK-LABEL: .Swappable1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Swappable1]Swapped.[Swappable1]Swapped, Self.[Swappable1]B == Self.[Swappable1]Swapped.[Swappable1]A, Self.[Swappable1]Swapped : Swappable1>
protocol Swappable1 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable1
    where Swapped.B == A,
          Swapped.A == B,
          Swapped.Swapped == Self
}

// CHECK-LABEL: .Swappable2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Swappable2]Swapped.[Swappable2]Swapped, Self.[Swappable2]B == Self.[Swappable2]Swapped.[Swappable2]A, Self.[Swappable2]Swapped : Swappable2>
protocol Swappable2 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable2
    where Swapped.B == A,
          Swapped.Swapped == Self
}

// CHECK-LABEL: .Swappable3@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Swappable3]Swapped.[Swappable3]Swapped, Self.[Swappable3]B == Self.[Swappable3]Swapped.[Swappable3]A, Self.[Swappable3]Swapped : Swappable3>
protocol Swappable3 {
  associatedtype A
  associatedtype B
  associatedtype Swapped : Swappable3
    where Swapped.A == B,
          Swapped.Swapped == Self
}
