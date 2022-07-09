// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: sr11997.(file).A@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[A]X.[B]Y, Self.[A]X : B>
protocol A {
  associatedtype X: B where X.Y == Self
}

// CHECK: sr11997.(file).B@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[B]Y.[A]X, Self.[B]Y : A>
protocol B {
  associatedtype Y: A where Y.X == Self
}

// CHECK: sr11997.(file).AA@
// CHECK-NEXT: Requirement signature: <Self where Self : A, Self.[A]X : BB>
protocol AA: A where X: BB { }

// CHECK: sr11997.(file).BB@
// CHECK-NEXT: Requirement signature: <Self where Self : B, Self == Self.[BB]Z.[C]T, Self.[B]Y : AA, Self.[BB]Z : C>
protocol BB: B where Y: AA {
  associatedtype Z: C where Z.T == Self
}

// CHECK: sr11997.(file).C@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[C]T.[BB]Z, Self.[C]T : BB>
protocol C {
  associatedtype T: BB where T.Z == Self
}

