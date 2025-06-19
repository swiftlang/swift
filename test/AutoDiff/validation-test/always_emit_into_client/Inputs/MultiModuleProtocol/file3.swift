import MultiModuleProtocol1
import MultiModuleProtocol2
import _Differentiation

public struct Struct : Protocol {
  private var _x : Float
  public  var x : Float {
    get { _x }
    set { _x = newValue }
  }
  public init() { _x = 0 }
}

extension Struct : AdditiveArithmetic {
  public static func +(lhs: Self, rhs: Self) -> Self { Self(lhs.x + rhs.x) }
  public static func -(lhs: Self, rhs: Self) -> Self { Self(lhs.x - rhs.x) }
  public static func +=(a: inout Self, b: Self) { a.x = a.x + b.x }
  public static func -=(a: inout Self, b: Self) { a.x = a.x - b.x }
  public static var zero: Self { Self(0) }
}

extension Struct : Differentiable {
  public  typealias TangentVector = Self
}
