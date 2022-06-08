// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// The rule
//
//   [P:Y].[concrete: G<τ_0_0, τ_0_1> with <[P:Z1], [P:Z2]>] => [P:Y]
//
// overlaps with [P].[P:Y] => [P], which applies the adjustment prepending
// [P] to [P:Z1] and [P:Z2], respectively.
//
// This produces the rule
//
//   [P:Y].[concrete: G<τ_0_0, τ_0_1> with <[P].[P:Z1], [P].[P:Z2]>] => [P:Y]
//
// When adding the rule, we have to simplify the concrete substitutions to
// reduce [P].[P:Z1] to [P:Z1] and [P].[P:Z2] to [P:Z2], respectively.

// CHECK-LABEL: simplify_concrete_substitutions.(file).P@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[P]X.[P]X, Self.[P]X : P, Self.[P]Y == G<Self.[P]Z1, Self.[P]Z2>>

protocol P {
  associatedtype X : P where X.X == Self
  associatedtype Y where Y == G<Z1, Z2>
  associatedtype Z1
  associatedtype Z2
}

struct G<T, U> {}
