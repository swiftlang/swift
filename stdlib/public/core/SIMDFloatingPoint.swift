/*

public protocol SIMDFloatingPointVector : SIMDVector,
                                          ExpressibleByIntegerLiteral,
                                          ExpressibleByFloatLiteral
                                    where Element: BinaryFloatingPoint {
  
  static func <(lhs: Self, rhs: Self) -> Predicate
  
  static func <=(lhs: Self, rhs: Self) -> Predicate
  
  static func >(lhs: Self, rhs: Self) -> Predicate
  
  static func >=(lhs: Self, rhs: Self) -> Predicate
  
  static prefix func -(rhs: Self) -> Self
  
  static func +(lhs: Self, rhs: Self) -> Self
  
  static func -(lhs: Self, rhs: Self) -> Self
  
  static func *(lhs: Self, rhs: Self) -> Self
  
  static func /(lhs: Self, rhs: Self) -> Self
  
  static func abs(_ value: Self) -> Self
  
  static func min(_ lhs: Self, _ rhs: Self) -> Self
  
  static func max(_ lhs: Self, _ rhs: Self) -> Self
  
  var isZero: Predicate { get }
  
  var isSubnormal: Predicate { get }
  
  var isNormal: Predicate { get }
  
  var isFinite: Predicate { get }
  
  var isInfinite: Predicate { get }
  
  var isNaN: Predicate { get }
  
  mutating func round(_ rule: FloatingPointRoundingRule)
  
  func rounded(_ rule: FloatingPointRoundingRule) -> Self
}

public extension SIMDFloatingPointVector {
  
  @_transparent
  init(integerLiteral: Int) {
    self.init(repeating: Element(integerLiteral))
  }
  
  @_transparent
  init(floatLiteral: Element) {
    self.init(repeating: floatLiteral)
  }
  
  @_transparent
  static func *(lhs: Self, rhs: Element) -> Self {
    return lhs * Self(repeating: rhs)
  }
  
  @_transparent
  static func *(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) * rhs
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Element) -> Self {
    return lhs / Self(repeating: rhs)
  }
  
  @_transparent
  static func +=(lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }
  
  @_transparent
  static func -=(lhs: inout Self, rhs: Self) {
    lhs = lhs - rhs
  }
  
  @_transparent
  static func *=(lhs: inout Self, rhs: Self) {
    lhs = lhs * rhs
  }
  
  @_transparent
  static func *=(lhs: inout Self, rhs: Element) {
    lhs = lhs * rhs
  }
  
  @_transparent
  static func /=(lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }
  
  @_transparent
  static func /=(lhs: inout Self, rhs: Element) {
    lhs = lhs / rhs
  }
  
  @_transparent
  static func min(_ lhs: Self, _ rhs: Self) -> Self {
    return lhs.replacing(with: rhs, where: rhs < lhs || lhs.isNaN)
  }
  
  @_transparent
  static func max(_ lhs: Self, _ rhs: Self) -> Self {
    return rhs.replacing(with: lhs, where: rhs < lhs || rhs.isNaN)
  }
  
  @_transparent
  var isZero: Predicate {
    return self == 0
  }
  
  @_transparent
  var isSubnormal: Predicate {
    let tiny = Self.abs(self) < Self(repeating: Element.leastNormalMagnitude)
    return tiny && !isZero
  }
  
  @_transparent
  var isNormal: Predicate {
    let tiny = Self.abs(self) < Self(repeating: Element.leastNormalMagnitude)
    return isFinite && !tiny
  }
  
  @_transparent
  var isFinite: Predicate {
    return Self.abs(self) < Self(repeating: Element.infinity)
  }
  
  @_transparent
  var isInfinite: Predicate {
    return Self.abs(self) == Self(repeating: Element.infinity)
  }
  
  @_transparent
  var isNaN: Predicate { return self != self }
  
  @_transparent
  mutating func round( ) {
    round(.toNearestOrEven)
  }
  
  @_transparent
  func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var other = self
    other.round(rule)
    return other
  }
  
  @_transparent
  func rounded( ) -> Self {
    return self.rounded(.toNearestOrEven)
  }
}

// MARK: Free functions implemented on the SIMDIntegerVector
@_transparent
public func abs<V>(_ value: V) -> V where V: SIMDFloatingPointVector {
  return V.abs(value)
}

@_transparent
public func min<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDFloatingPointVector {
  return V.min(lhs, rhs)
}

@_transparent
public func max<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDFloatingPointVector {
  return V.max(lhs, rhs)
}

@_transparent
public func clamp<V>(_ value: V, min: V, max: V) -> V where V: SIMDFloatingPointVector {
  return V.min(V.max(value, min), max)
}

 */
