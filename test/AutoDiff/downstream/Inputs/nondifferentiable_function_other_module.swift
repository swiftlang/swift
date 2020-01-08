// A public function that is not marked with `@differentiable`.
// Differentiation of `externalFunction` in other modules should fail.
public func externalFunction(_ x: Float) -> Float {
  return x + x
}
