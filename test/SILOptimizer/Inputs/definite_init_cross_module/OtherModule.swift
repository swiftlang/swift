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

open class VisibleNoArgsDesignatedInit {
  var x: Float
  public init() { x = 0.0 }

  // Add some designated inits the subclass cannot see.
  private init(x: Float) { self.x = x }
  fileprivate init(y: Float) { self.x = y }
  internal init(z: Float) { self.x = z }
}

