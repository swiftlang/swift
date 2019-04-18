public protocol Layer : Differentiable {
  @differentiable
  call func(_ input: Float) -> Float
}

public struct Dense : Differentiable {
  public init() {}

  @differentiable
  public call func(_ input: Float) -> Float {
    return input * 2
  }
}
