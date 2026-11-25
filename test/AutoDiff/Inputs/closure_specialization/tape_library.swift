import _Differentiation

@inlinable
@differentiable(reverse, wrt: x)
public func importedBranch<T>(_ x: Float, _ marker: T, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}
