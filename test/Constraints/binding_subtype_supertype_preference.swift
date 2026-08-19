// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-constraints-on-line=25 2>&1 | %FileCheck %s

// This expression first appeared in test/SILOptimizer/sil_combine1.swift,
// where it was intended to test a specific SIL optimization, with the
// T1, T2, and T3 generic parameters of curry() bound to 'any P'.
//
// The SIL test now adds an explicit type annotation, but here we test the
// new behavior, where we prefer to bind a type variable to the most specific
// type if possible, without attempting both the lower bound and upper bound
// when they are different.
//
// In this example, this means we find the solution with a function conversion,
// where T1 and T2 are 'S', and T3 is 'any P', even though this has a worse
// score than the solution where all three are 'any P', because we do not
// attempt those possibilities at all.

func curry<T1, T2, T3, T4>(_: (T1, T2, T3) -> T4) -> (T1) -> (T2) -> (T3) -> T4 {}
func compose(_: any P, _: any P, _: any P) -> Int32 { 0 }

public protocol P {}
struct S: P {}

public func test() {
  let _ = curry(compose)(S())(S())
}

// CHECK-LABEL: ---Solution---
// CHECK-NEXT: Fixed score: [function conversion(s), weight: 11, impact: 2] [use of overloaded unapplied function(s), weight: 1, impact: 1]
// CHECK: Type variables:
// CHECK: $T0 as ((S, S, any P) -> Int32) -> (S) -> (S) -> (any P) -> Int32