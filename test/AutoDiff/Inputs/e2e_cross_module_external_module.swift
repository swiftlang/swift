import DifferentiationUnittest

@differentiable
public func doubleThenApplyDefaultF(_ x: Tracked<Float>) -> Tracked<Float> {
  return x
}

@differentiable
public func doubleThenApply(
  _ x: Tracked<Float>,
  _ f: @differentiable (Tracked<Float>) -> Tracked<Float> = doubleThenApplyDefaultF
) -> Tracked<Float> {
  return f(2 * x)
}
