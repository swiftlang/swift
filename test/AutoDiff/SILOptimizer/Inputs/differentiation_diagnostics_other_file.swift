import _Differentiation

protocol Protocol: Differentiable {
  // Test cross-file `@differentiable` attribute.
  @differentiable(wrt: self)
  func identityDifferentiableAttr() -> Self
}

extension Protocol {
  func identityDerivativeAttr() -> Self { self }

  // Test cross-file `@derivative` attribute.
  @derivative(of: identityDerivativeAttr)
  func vjpIdentityDerivativeAttr() -> (
    value: Self, pullback: (TangentVector) -> TangentVector
  ) {
    fatalError()
  }
}
