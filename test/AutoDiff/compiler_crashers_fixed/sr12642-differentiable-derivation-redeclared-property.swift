// RUN: %target-swift-frontend -typecheck -verify %s
// REQUIRES: asserts

// SR-12642: Crash regarding `Differentiable` derived conformances and
// redeclared properties. This crash surfaced only briefly during the
// implementation of wrapped property differentiation (SR-12637).

import _Differentiation

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct Generic<T> {}
extension Generic: Differentiable where T: Differentiable {}

struct WrappedProperties: Differentiable {
  @Wrapper var int: Generic<Int>
  // expected-note@-1 {{'int' previously declared here}}
  // expected-note@-2 {{'_int' synthesized for property wrapper backing storage}}
  // expected-warning@-3 {{stored property 'int' has no derivative because 'Generic<Int>' does not conform to 'Differentiable'; add an explicit '@noDerivative' attribute}}

  @Wrapper var int: Generic<Int>
  // expected-error@-1 {{invalid redeclaration of 'int'}}
  // expected-error@-2 {{invalid redeclaration of synthesized property '_int'}}
}
