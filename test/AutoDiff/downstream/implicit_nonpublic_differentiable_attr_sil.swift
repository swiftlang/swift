// RUN: %target-swift-frontend -emit-sil -verify %s

// Test end-to-end differentiation involving implicit `@differentiable`
// attributes for non-public protocol witnesses.
//
// Specifically, test the diagnostic source locations for implicit attributes.

protocol Protocol: Differentiable {
  // Note: error below comes from the implicit `@differentiable` attribute on
  // `PublicConformingStruct.internalMethod`. The source location of the
  // implicit attribute is copied from the protocol requirement's attribute.

  // expected-error @+1 {{function is not differentiable}}
  @differentiable(wrt: (self, x))
  func internalMethod(_ x: Float) -> Float
}

struct ConformingStruct: Protocol {
  // Expected:
  // - No error for missing `@differentiable` attribute on internal protocol witness.
  //   An implicit `@differentiable` attribute should be created.
  // - A non-differentiability error, because the method body is non-differentiable.
  // expected-note @+1 {{when differentiating this function definition}}
  func internalMethod(_ x: Float) -> Float {
    // expected-note @+1 {{cannot differentiate through a non-differentiable result; do you want to use 'withoutDerivative(at:)'?}}
    return Float(Int(x))
  }
}
