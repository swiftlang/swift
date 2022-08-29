// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/49901

// CHECK-LABEL: sr7353.(file).P@
// CHECK-LABEL: <Self where Self == Self.[P]A.[Q]B, Self.[P]A : Q>
protocol P {
  associatedtype A: Q where A.B == Self
}

// CHECK-LABEL: sr7353.(file).Q@
// CHECK-LABEL: <Self where Self == Self.[Q]B.[P]A, Self.[Q]B : P, Self.[Q]B == Self.[Q]C>
protocol Q {
  associatedtype B: P where B.A == Self
  associatedtype C: P where C.A == Self
}
