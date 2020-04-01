// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -typecheck -verify %s
// REQUIRES: asserts

// TF-1167: `OverrideMatcher::match` crash due to meaningless assertion:
// `assert(false)`. The assertion was triggered when parameter indices
// could not be resolved for neither base nor derived declaration
// `@differentiable` attributes.
//
// `import _Differentiation` is intentionally omitted from this test case.

public protocol Base {
  associatedtype Input
  // expected-error @+1 {{use of undeclared type 'Differentiable'}}
  associatedtype Output: Differentiable

  // expected-error @+1 {{@differentiable attribute used without importing module '_Differentiation'}}
  @differentiable(wrt: self)
  func callAsFunction(_ input: Input) -> Output
}
public protocol Derived: Base {
  // expected-error @+1 {{@differentiable attribute used without importing module '_Differentiation'}}
  @differentiable
  func callAsFunction(_ input: Input) -> Output
}
