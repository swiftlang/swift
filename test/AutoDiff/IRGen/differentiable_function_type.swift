// RUN: %target-swift-frontend -emit-ir -g %s -requirement-machine=off

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
