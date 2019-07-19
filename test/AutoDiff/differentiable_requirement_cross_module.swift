// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/differentiable_requirement_other_module.swift -emit-module-path %t/differentiable_requirement_other_module.swiftmodule
// RUN: %target-swift-frontend -typecheck -I %t -primary-file %s -verify

import differentiable_requirement_other_module

// Conform `Empty` to `Differentiable`.
// The `foo` protocol requirement is `@differentiable` and has an `Empty` parameter.
extension Empty : Differentiable {
  public typealias TangentVector = Empty
  public typealias AllDifferentiableVariables = Empty
}

// expected-error @+2 {{type 'Conforming' does not conform to protocol 'DifferentiableRequirement'}}
// expected-note @+1 {{do you want to add protocol stubs?}}
struct Conforming : DifferentiableRequirement {
  // expected-note @+1 {{candidate is missing attribute '@differentiable(wrt: float)'}}
  func foo(float: Float, empty: Empty) -> Float {
    return float
  }
}
