// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature BuiltinModule -emit-silgen -swift-version 6 %s

// https://github.com/swiftlang/swift/issues/92296
// Ensure we skip @Sendable conversions when checking for builtin name
import Builtin
import _Differentiation

public func valueWithPullback<T, R>(
  at x: T, of f: @differentiable(reverse) (T) -> R
) -> (value: R, pullback: (R.TangentVector) -> T.TangentVector) {
  return Builtin.applyDerivative_vjp(f, x)
}

public func valueWithPullback1<T0, R>(
    at x0: T0,
    of f: @differentiable(reverse) (T0) -> R
) -> (value: R, pullback: (R.TangentVector) -> T0.TangentVector)
    where T0: Differentiable, R: Differentiable
{
    Builtin.applyDerivative_vjp_arity1(f, x0)
}
