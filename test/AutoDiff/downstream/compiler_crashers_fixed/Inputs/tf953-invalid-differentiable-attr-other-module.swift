// Invalid `@differentiable` attribute in non-primary-file should not crash
// SILGen.
@differentiable
func foo(_ x: Int) -> Float {
  return Float(x)
}
