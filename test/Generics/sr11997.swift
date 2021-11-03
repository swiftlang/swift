// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr11997.(file).A@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.X.Y, Self.X : B>
protocol A {
  associatedtype X: B where X.Y == Self
}

// CHECK: sr11997.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Y.X, Self.Y : A>
protocol B {
  associatedtype Y: A where Y.X == Self
}

// CHECK: sr11997.(file).AA@
// CHECK-NEXT: Requirement signature: <Self where Self : A, Self.X : BB>
protocol AA: A where X: BB { }

// CHECK: sr11997.(file).BB@
// CHECK-NEXT: Requirement signature: <Self where Self : B, Self == Self.Z.T, Self.Y : AA, Self.Z : C>
protocol BB: B where Y: AA {
  associatedtype Z: C where Z.T == Self
}

// CHECK: sr11997.(file).C@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.T.Z, Self.T : BB>
protocol C {
  associatedtype T: BB where T.Z == Self
}

