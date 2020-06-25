// RUN: %target-swift-frontend -typecheck -verify %s

protocol P {}

public protocol HasRequirement {
  @differentiable
  // expected-note @+1 {{protocol requires function 'requirement' with type '<T> (T, T) -> T'; do you want to add a stub?}}
  func requirement<T: Differentiable>(_ x: T, _ y: T) -> T
}

// expected-error @+1 {{type 'AttemptsToSatisfyRequirement' does not conform to protocol 'HasRequirement'}}
public struct AttemptsToSatisfyRequirement: HasRequirement {
  // This does not satisfy the requirement because the differentiable attribute is more
  // constrained than the requirement's differentiable attribute.
  @differentiable(where T: P)
  // expected-note @+1 {{candidate is missing attribute '@differentiable(wrt: (x, y))'}}
  public func requirement<T: Differentiable>(_ x: T, _ y: T) -> T { x }
}
