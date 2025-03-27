public struct Struct {
  public var x : Float
  public typealias TangentVector = Self
  public init() { x = 0 }
}

extension Struct {
  public init(_ val: Float) {
    self.init()
    x = val
  }

  @_alwaysEmitIntoClient
  public func sum() -> Float { x }
}

extension Struct : AdditiveArithmetic {
  public static func +(lhs: Self, rhs: Self) -> Self { Self(lhs.x + rhs.x) }
  public static func -(lhs: Self, rhs: Self) -> Self { Self(lhs.x - rhs.x) }
  public static func +=(a: inout Self, b: Self) { a.x = a.x + b.x }
  public static func -=(a: inout Self, b: Self) { a.x = a.x - b.x }
  public static var zero: Self { Self(0) }
}
