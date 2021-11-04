// RUN: %target-build-swift %s

import _Differentiation

public protocol Layer {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  @differentiable(reverse)
  func callAsFunction(_ input: Input) -> Output
}

public class Function<Input: Differentiable, Output: Differentiable>: Layer {
  @noDerivative public let body: @differentiable(reverse) (Input) -> Output

  public init(_ body: @escaping @differentiable(reverse) (Input) -> Output) {
    self.body = body
  }

  @differentiable(reverse)
  public func callAsFunction(_ input: Input) -> Output {
    body(input)
  }
}
