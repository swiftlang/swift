public protocol Layer {
  func callFunction(_ input: Float) -> Float
}

public struct Dense {
  public init() {}

  public func callFunction(_ input: Float) -> Float {
    return input * 2
  }
}
