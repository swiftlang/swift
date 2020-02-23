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

private struct PrivateConforming : DifferentiableRequirement {
  fileprivate func foo(float: Float, empty: Empty) -> Float {
    return float
  }
}

struct InternalConforming : DifferentiableRequirement {
  func foo(float: Float, empty: Empty) -> Float {
    return float
  }
}

// expected-error @+1 {{type 'PublicConforming' does not conform to protocol 'DifferentiableRequirement'}}
public struct PublicConforming : DifferentiableRequirement {
  // expected-note @+1 {{candidate is missing attribute '@differentiable(wrt: float)'}}
  public func foo(float: Float, empty: Empty) -> Float {
    return float
  }
}
