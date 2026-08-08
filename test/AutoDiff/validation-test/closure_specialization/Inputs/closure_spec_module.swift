import _Differentiation

@differentiable(reverse)
public func mybar1(_ x: Float) -> Float {
  x
}

@differentiable(reverse)
public func mybar2(_ x: Float) -> Float {
  x * x
}
