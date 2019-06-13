// SWIFT_ENABLE_TENSORFLOW

public protocol Layer : Differentiable {
  @differentiable
  func callAsFunction(_ input: Float) -> Float
}

public struct Dense : Differentiable {
  public init() {}

  @differentiable
  public func callAsFunction(_ input: Float) -> Float {
    return input * 2
  }
}
