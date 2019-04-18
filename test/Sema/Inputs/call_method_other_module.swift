public protocol Layer : Differentiable {
  @differentiable
  func call(_ input: Float) -> Float
}

public struct Dense : Differentiable {
  public init() {}

  @differentiable
  public func call(_ input: Float) -> Float {
    return input * 2
  }
}
