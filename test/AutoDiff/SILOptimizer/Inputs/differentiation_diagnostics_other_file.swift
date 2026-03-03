import _Differentiation

protocol Protocol: Differentiable {
  // Test cross-file `@differentiable` attribute.
  @differentiable(reverse, wrt: self)
  func identityDifferentiableAttr() -> Self

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable(reverse)
  var property: Float { get set }

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable(reverse)
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

struct Struct: Differentiable {
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
  @differentiable(reverse)
  var property: Float {
    get { 1 }
    set {}
  }

  // Test `@differentiable` propagation from storage declaration to accessors.
  @differentiable(reverse)
  subscript() -> Float {
    get { 1 }
    set {}
  }
}

struct S: Differentiable {
  var value: Float
}

extension Array where Element == S {
  @differentiable(reverse)
  func sum() -> Float {
    return 0
  }
}
