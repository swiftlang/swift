// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// This is a funny presentation of the symmetric group on 4 elements
// from Joe Groff.

// CHECK-LABEL: symmetric_group_4.(file).O1@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O1, Self.R : O1, Self.C.C.C.C == Self.C.C.R.C.C.R, Self.C.C.R.C.C.R == Self.R.C.R.C.R.C>
protocol O1 {
  associatedtype R : O1
  associatedtype C : O1
    where C.C.C.C == Self,
          C.C.R.C.C.R == Self,
          R.C.R.C.R.C == Self
}

// CHECK-LABEL: symmetric_group_4.(file).O2@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O2, Self.R : O2, Self.C.C.C.C == Self.C.C.R.C.C.R, Self.C.C.R.C.C.R == Self.R.C.R.C.R.C>
protocol O2 {
  associatedtype R : O2
  associatedtype C : O2
    where Self == C.C.C.C,
          C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C
}

// The GSB incorrectly minimized this one, dropping all of the same-type requirements.

// CHECK-LABEL: symmetric_group_4.(file).O3@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.C.C.C.C, Self.C : O3, Self.R : O3, Self.C.C.C.C == Self.C.C.R.C.C.R, Self.C.C.R.C.C.R == Self.R.C.R.C.R.C>
protocol O3 {
  associatedtype R : O3
  associatedtype C : O3
    where C.C.C.C == C.C.R.C.C.R,
          C.C.R.C.C.R == R.C.R.C.R.C,
          R.C.R.C.R.C == Self
}
