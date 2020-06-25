public struct Empty : AdditiveArithmetic {}

public protocol DifferentiableRequirement {
  @differentiable
  func foo(float: Float, empty: Empty) -> Float
}
