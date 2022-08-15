// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// This is a funny monoid presentation from Joe Groff.

// CHECK-LABEL: joe_monoid.(file).O1@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[O1]C.[O1]C.[O1]C.[O1]C, Self.[O1]C : O1, Self.[O1]R : O1, Self.[O1]C.[O1]C == Self.[O1]R.[O1]C.[O1]C.[O1]R, Self.[O1]C.[O1]R.[O1]C == Self.[O1]R.[O1]C.[O1]R>
protocol O1 {
  associatedtype R : O1
  associatedtype C : O1
    where C.C.C.C == Self,
          C.C.R.C.C.R == Self,
          R.C.R.C.R.C == Self
}

// CHECK-LABEL: joe_monoid.(file).O2@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[O2]C.[O2]C.[O2]C.[O2]C, Self.[O2]C : O2, Self.[O2]R : O2, Self.[O2]C.[O2]C == Self.[O2]R.[O2]C.[O2]C.[O2]R, Self.[O2]C.[O2]R.[O2]C == Self.[O2]R.[O2]C.[O2]R>
protocol O2 {
  associatedtype R : O2
  associatedtype C : O2
    where Self == C.C.C.C,
          C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C
}

// The GSB incorrectly minimized this one, dropping all of the same-type requirements.

// CHECK-LABEL: joe_monoid.(file).O3@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[O3]C.[O3]C.[O3]C.[O3]C, Self.[O3]C : O3, Self.[O3]R : O3, Self.[O3]C.[O3]C == Self.[O3]R.[O3]C.[O3]C.[O3]R, Self.[O3]C.[O3]R.[O3]C == Self.[O3]R.[O3]C.[O3]R>
protocol O3 {
  associatedtype R : O3
  associatedtype C : O3
    where C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C,
          R.C.R.C.R.C == Self
}

// CHECK-LABEL: joe_monoid.(file).O4@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[O4]C.[O4]C.[O4]C.[O4]C, Self.[O4]C : O4, Self.[O4]R : O4, Self.[O4]C.[O4]C == Self.[O4]R.[O4]C.[O4]C.[O4]R, Self.[O4]C.[O4]R.[O4]C == Self.[O4]R.[O4]C.[O4]R>
protocol O4 {
  associatedtype R : O4
  associatedtype C : O4
    where Self == C.C.C.C,
          C.C == R.C.C.R,
          C.R.C == R.C.R
}
