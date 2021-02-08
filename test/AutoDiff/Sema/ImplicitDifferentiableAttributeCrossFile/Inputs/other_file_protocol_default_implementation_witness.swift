import _Differentiation

protocol P1: Differentiable {
  @differentiable(reverse, wrt: self)
  // expected-note @+1 {{protocol requires function 'callAsFunction' with type '(Float) -> Float'}}
  func callAsFunction(_ input: Float) -> Float
}

protocol P2: P1 {}

extension P2 {
  @differentiable(reverse, wrt: (self, input))
  // expected-note @+1 {{candidate is missing explicit '@differentiable(reverse, wrt: self)' attribute to satisfy requirement 'callAsFunction' (in protocol 'P1'); explicit attribute is necessary because candidate is declared in a different type context or file than the conformance of 'ConformingStruct' to 'P1'}}
  public func callAsFunction(_ input: Float) -> Float {
    return input
  }
}
