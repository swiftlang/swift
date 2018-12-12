import Swift

@inlinable
public func valueWithDifferential<T, R>(
  at x: T, in f: @autodiff (T) -> R
) -> (value: R, differential: (T.TangentVector) -> R.TangentVector)
  where T : Differentiable, R : Differentiable {
  return withoutActuallyEscaping(f) { f in
    Builtin.autodiffGetJVP(f)(x)
  }
}
