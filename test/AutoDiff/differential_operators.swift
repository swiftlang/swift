// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck %s

import Swift

@inlinable
public func valueWithDifferential<T, R>(
  at x: T, in f: @escaping @autodiff (T) -> R
) -> (value: R, differential: (T.TangentVector) -> R.TangentVector)
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffGetJVP(f)(x)
}

@inlinable
public func valueWithPullback<T, R>(
  at x: T, in f: @escaping @autodiff (T) -> R
) -> (value: R, pullback: (R.CotangentVector) -> T.CotangentVector)
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffGetVJP(f)(x)
}
