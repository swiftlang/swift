import _Differentiation

@differentiable
public func defaultArgument(_ x: Float) -> Float {
  return x
}

@differentiable
public func applyArgument(
  _ x: Float,
  _ f: @differentiable (Float) -> Float = defaultArgument
) -> Float {
  return f(x)
}
