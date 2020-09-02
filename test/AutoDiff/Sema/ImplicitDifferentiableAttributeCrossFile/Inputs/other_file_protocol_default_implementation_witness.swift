import _Differentiation

protocol P1: Differentiable {
  @differentiable(wrt: self)
  // expected-note @+1 {{protocol requires function 'callAsFunction' with type '(Float) -> Float'}}
  func callAsFunction(_ input: Float) -> Float
}

protocol P2: P1 {}

extension P2 {
  @differentiable(wrt: (self, input))
  // expected-note @+1 {{instance method 'callAsFunction' must have explicit '@differentiable(wrt: self)' attribute to satisfy requirement instance method 'callAsFunction' (in protocol 'P1') because it is declared in a different file than the conformance of 'ConformingStruct' to 'P1'}}
  public func callAsFunction(_ input: Float) -> Float {
    return input
  }
}
