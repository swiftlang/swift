import _Differentiation

@differentiable(reverse)
public func defaultArgument(_ x: Float) -> Float {
  return x
}

@differentiable(reverse)
public func applyArgument(
  _ x: Float,
  _ f: @differentiable(reverse) (Float) -> Float = defaultArgument
) -> Float {
  return f(x)
}
