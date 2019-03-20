public struct Empty {
  public init() {}
}

public protocol DifferentiableRequirement {
  @differentiable
  func foo(float: Float, empty: Empty) -> Float
}
