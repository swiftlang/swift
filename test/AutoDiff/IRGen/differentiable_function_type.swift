// RUN: %target-swift-frontend -emit-ir -g %s

// TF-1225: IRGenDebugInfo crash when reconstructing `@differentiable` and
// `@differentiable(linear)` function types.

import _Differentiation

@inlinable
public func transpose<T, R>(
  of body: @escaping @differentiable(linear) (T) -> R
) -> @differentiable(linear) (R) -> T {
  fatalError()
}

@inlinable
public func valueWithDifferential<T, R>(
  at x: T, in f: @differentiable (T) -> R
) -> (value: R, differential: (T.TangentVector) -> R.TangentVector) {
  fatalError()
}
