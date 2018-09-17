public protocol SIMDFloatingPointVector : SIMDVector
                          where Element : BinaryFloatingPoint {

  associatedtype BitPattern: SIMDIntegerVector
    where BitPattern.Predicate == Predicate

  var bitPattern: BitPattern { get }
  
  init(bitPattern: BitPattern)
  
  static func <(lhs: Self, rhs: Self) -> Predicate
  
  static func <=(lhs: Self, rhs: Self) -> Predicate
  
  static func >(lhs: Self, rhs: Self) -> Predicate
  
  static func >=(lhs: Self, rhs: Self) -> Predicate
  
  static func +(lhs: Self, rhs: Self) -> Self
  
  static func -(lhs: Self, rhs: Self) -> Self
  
  static func *(lhs: Self, rhs: Self) -> Self
  
  static func /(lhs: Self, rhs: Self) -> Self
  
  func addingProduct(_ lhs: Self, _ rhs: Self) -> Self
  
  func squareRoot( ) -> Self
  
  mutating func round(_ rule: FloatingPointRoundingRule)
}

public extension SIMDFloatingPointVector {
  
  // MARK: Vector-scalar comparisons
  @inlinable
  static func ==(lhs: Self, rhs: Element) -> Predicate {
    return lhs == Self(repeating: rhs)
  }
  
  @inlinable
  static func !=(lhs: Self, rhs: Element) -> Predicate {
    return lhs != Self(repeating: rhs)
  }

  @inlinable
  static func <(lhs: Self, rhs: Element) -> Predicate {
    return lhs < Self(repeating: rhs)
  }
  
  @inlinable
  static func <=(lhs: Self, rhs: Element) -> Predicate {
    return lhs <= Self(repeating: rhs)
  }
  
  @inlinable
  static func >(lhs: Self, rhs: Element) -> Predicate {
    return lhs > Self(repeating: rhs)
  }
  
  @inlinable
  static func >=(lhs: Self, rhs: Element) -> Predicate {
    return lhs >= Self(repeating: rhs)
  }
  
  @inlinable
  static func ==(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) == rhs
  }
  
  @inlinable
  static func !=(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) != rhs
  }
  
  @inlinable
  static func <(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) < rhs
  }
  
  @inlinable
  static func <=(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) <= rhs
  }
  
  @inlinable
  static func >(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) > rhs
  }
  
  @inlinable
  static func >=(lhs: Element, rhs: Self) -> Predicate {
    return Self(repeating: lhs) >= rhs
  }
  
  // MARK: Arithmetic operators
  @_transparent
  static func +(lhs: Self, rhs: Element) -> Self {
    return lhs + Self(repeating: rhs)
  }
  
  @_transparent
  static func +(lhs: Element, rhs: Self) -> Self {
    return rhs + lhs
  }
  
  @_transparent
  static func -(lhs: Self, rhs: Element) -> Self {
    return lhs - Self(repeating: rhs)
  }
  
  @_transparent
  static func -(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) - rhs
  }
  
  @_transparent
  static func *(lhs: Self, rhs: Element) -> Self {
    return lhs * Self(repeating: rhs)
  }
  
  @_transparent
  static func *(lhs: Element, rhs: Self) -> Self {
    return rhs * lhs
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Element) -> Self {
    return lhs / Self(repeating: rhs)
  }
  
  @_transparent
  static func /(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) / rhs
  }
  
  // MARK: In-place arithmetic operators
  @_transparent
  static func +=(lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }
  
  @_transparent
  static func +=(lhs: inout Self, rhs: Element) {
    lhs = lhs + rhs
  }
  
  @_transparent
  static func -=(lhs: inout Self, rhs: Self) {
    lhs = lhs - rhs
  }
  
  @_transparent
  static func -=(lhs: inout Self, rhs: Element) {
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
  
  // MARK: Arithmetic functions
  @_transparent
  func addingProduct(_ lhs: Element, _ rhs: Self) -> Self {
    return self.addingProduct(Self(repeating: lhs), rhs)
  }
  
  @_transparent
  func addingProduct(_ lhs: Self, _ rhs: Element) -> Self {
    return self.addingProduct(lhs, Self(repeating: rhs))
  }
  
  @_transparent
  mutating func addProduct(_ lhs: Self, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent
  mutating func addProduct(_ lhs: Element, _ rhs: Self) {
    self = self.addingProduct(lhs, rhs)
  }
  
  @_transparent
  mutating func addProduct(_ lhs: Self, _ rhs: Element) {
    self = self.addingProduct(lhs, rhs)
  }
  
  // TODO: remove once we have importer support and provide the correct
  // definitions via C shims (the sqrt intrinsic does not have the right
  // semantics for our use).
  @inlinable
  func squareRoot( ) -> Self {
    var result = Self()
    for i in indices { result[i] = self[i].squareRoot() }
    return result
  }
  
  @_transparent
  func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var result = self
    result.round(rule)
    return result
  }
  
  /// A vector where the value in each lane is selected from the corresponding
  /// lane of `self` (if that lane of `predicate` is `false`) or `other` (if
  /// that lane of `predicate` is `true`).
  @_transparent
  func replacing(with other: Self, where predicate: Predicate) -> Self {
    return Self(bitPattern:
      self.bitPattern.replacing(with: other.bitPattern, where: predicate)
    )
  }
}
