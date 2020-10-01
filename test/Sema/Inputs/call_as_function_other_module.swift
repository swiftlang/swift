public protocol Layer {
  func callAsFunction(_ input: Float) -> Float
}

public struct Dense {
  public init() {}

  public func callAsFunction(_ input: Float) -> Float {
    return input * 2
  }
}
