// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// This is a funny monoid presentation from Joe Groff.

// CHECK-LABEL: joe_monoid.(file).O1@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O1, Self.R : O1, Self.C.C == Self.R.C.C.R, Self.C.R.C == Self.R.C.R>
protocol O1 {
  associatedtype R : O1
  associatedtype C : O1
    where C.C.C.C == Self,
          C.C.R.C.C.R == Self,
          R.C.R.C.R.C == Self
}

// CHECK-LABEL: joe_monoid.(file).O2@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O2, Self.R : O2, Self.C.C == Self.R.C.C.R, Self.C.R.C == Self.R.C.R>
protocol O2 {
  associatedtype R : O2
  associatedtype C : O2
    where Self == C.C.C.C,
          C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C
}

// The GSB incorrectly minimized this one, dropping all of the same-type requirements.

// CHECK-LABEL: joe_monoid.(file).O3@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O3, Self.R : O3, Self.C.C == Self.R.C.C.R, Self.C.R.C == Self.R.C.R>
protocol O3 {
  associatedtype R : O3
  associatedtype C : O3
    where C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C,
          R.C.R.C.R.C == Self
}

// CHECK-LABEL: joe_monoid.(file).O4@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O4, Self.R : O4, Self.C.C == Self.R.C.C.R, Self.C.R.C == Self.R.C.R>
protocol O4 {
  associatedtype R : O4
  associatedtype C : O4
    where Self == C.C.C.C,
          C.C == R.C.C.R,
          C.R.C == R.C.R
}
