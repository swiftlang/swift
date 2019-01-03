// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{@noDerivative is only allowed on stored properties in structure types that declare a conformance to 'Differentiable'}}
@noDerivative var flag: Bool

struct Foo {
  // expected-error @+1 {{@noDerivative is only allowed on stored properties in structure types that declare a conformance to 'Differentiable'}}
  @noDerivative var flag: Bool
}

// expected-error @+1 {{type 'Bar' does not conform to protocol 'Differentiable'}}
struct Bar : Differentiable {
  @noDerivative var flag: Bool
}
