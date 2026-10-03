// RUN: %target-swift-frontend -typecheck -verify %s

import _Differentiation

// Despite their similar construction and diagnostics, these two test cases
// exercise different parts of the generic system.

protocol RequiresPlainFn { associatedtype A where A == (Float) -> Float }
protocol RequiresDifferentiableFn { associatedtype A where A == (@differentiable(reverse) (Float) -> Float) }

func conflictingDifferentiability<T: RequiresPlainFn & RequiresDifferentiableFn>(_ t: T) {}
// expected-error@-1 {{no type for 'T.A' can satisfy both 'T.A == (Float) -> Float' and 'T.A == @differentiable(reverse) (Float) -> Float'}}

protocol RequiresBothViaMerge {
  // expected-error@-1 {{no type for 'Self.MergedA.A' can satisfy both 'Self.MergedA.A == (Float) -> Float' and 'Self.MergedA.A == @differentiable(reverse) (Float) -> Float'}}
  associatedtype MergedA : RequiresPlainFn & RequiresDifferentiableFn
}
