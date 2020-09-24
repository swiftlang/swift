import _Differentiation

protocol Protocol: Differentiable {
  // Test cross-file `@differentiable` attribute.
  @differentiable(wrt: self)
  func identityDifferentiableAttr() -> Self

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable
  var property: Float { get set }

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable
  subscript() -> Float { get set }
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

class Class: Differentiable {
  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable
  var property: Float {
    get { 1 }
    set {}
  }

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable
  subscript() -> Float {
    get { 1 }
    set {}
  }
}

struct S: Differentiable {
  var value: Float
}

extension Array where Element == S {
  @differentiable
  func sum() -> Float {
    return 0
  }
}
