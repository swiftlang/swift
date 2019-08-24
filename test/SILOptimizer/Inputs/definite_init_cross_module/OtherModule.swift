public struct Point {
  public var x, y: Double

  public init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
}

public struct ImmutablePoint {
  public let x, y: Double

  public init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
}

public struct GenericPoint<T> {
  public var x, y: T

  public init(x: T, y: T) {
    self.x = x
    self.y = y
  }
}

public struct PrivatePoint {
  private var x, y: Double

  public init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
}

public struct Empty {
  public init() {}
}

public struct GenericEmpty<T> {
  public init() {}
}
