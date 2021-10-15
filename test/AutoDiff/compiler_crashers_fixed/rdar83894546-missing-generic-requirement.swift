// RUN: %target-build-swift %s

import _Differentiation

public protocol Layer {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable
  func callAsFunction(_ input: Input) -> Output
}

public class Function<Input: Differentiable, Output: Differentiable>: Layer {
  public typealias Body = @differentiable(reverse) (Input) -> Output

  @noDerivative public let body: Body

  public init(_ body: @escaping Body) {
    self.body = body
  }

  @differentiable(reverse)
  public func callAsFunction(_ input: Input) -> Output {
    body(input)
  }
}
