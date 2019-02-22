// RUN: %target-swift-frontend -typecheck -verify %s

// expected-error @+1 {{'@noDerivative' is only allowed on stored properties in structure types that declare a conformance to 'Differentiable'}}
@noDerivative var flag: Bool

struct Foo {
  // expected-error @+1 {{'@noDerivative' is only allowed on stored properties in structure types that declare a conformance to 'Differentiable'}}
  @noDerivative var flag: Bool
}

struct Bar : Differentiable {
  @noDerivative var flag: Bool
}

// Test TF-152: derived conformances "no interface type set" crasher.
struct TF_152: Differentiable {
  @differentiable(wrt: bar)
  func applied(to input: Float, bar: TF_152_Bar) -> Float {
    return input
  }
}
struct TF_152_Bar: Differentiable {
  @noDerivative let dense: Float
}
