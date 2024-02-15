// RUN: %target-swift-frontend -emit-ir -g %s \
// RUN: -disable-round-trip-debug-types

// FIXME: round-trip-debug-types is disabled for this test because it fails on Windows:
// Incorrect reconstructed type for $sq_xIelgnr_D
//
// rdar://123029365 (AutoDiff/IRGen/differentiable_function_type.swift fails sporadically on Windows;
// blocks PR testing)

import _Differentiation

@inlinable
public func transpose<T, R>(
  of body: @escaping @differentiable(_linear) (T) -> R
) -> @differentiable(_linear) (R) -> T {
  fatalError()
}

@inlinable
public func valueWithPullback<T, R>(
  at x: T, in f: @differentiable(reverse) (T) -> R
) -> (value: R, pullback: (T.TangentVector) -> R.TangentVector) {
  fatalError()
}
