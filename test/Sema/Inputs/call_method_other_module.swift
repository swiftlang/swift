public protocol Layer {
  func call(_ input: Float) -> Float
}

public struct Dense {
  public init() {}

  public func call(_ input: Float) -> Float {
    return input * 2
  }
}
