public protocol SIMDFloatingPointVector : SIMDVector
                          where Element : BinaryFloatingPoint,
                 Element.RawSignificand : FixedWidthInteger {
  
  static var zero: Self { get }
  
  static func <(lhs: Self, rhs: Self) -> Mask
  
  static func <=(lhs: Self, rhs: Self) -> Mask
  
  static func >(lhs: Self, rhs: Self) -> Mask
  
  static func >=(lhs: Self, rhs: Self) -> Mask
  
  static func +(lhs: Self, rhs: Self) -> Self
  
  static func -(lhs: Self, rhs: Self) -> Self
  
  static func .*(lhs: Self, rhs: Self) -> Self
  
  static func /(lhs: Self, rhs: Self) -> Self
  
  func addingProduct(_ lhs: Self, _ rhs: Self) -> Self
  
  func squareRoot( ) -> Self
  
  mutating func round(_ rule: FloatingPointRoundingRule)
}

public extension SIMDFloatingPointVector {
  
  /// The zero vector.
  static var zero: Self { return Self() }
  
  // MARK: Vector-scalar comparisons
  @inlinable
  static func .==(lhs: Self, rhs: Element) -> Mask {
    return lhs .== Self(repeating: rhs)
  }
  
  @inlinable
  static func .!=(lhs: Self, rhs: Element) -> Mask {
    return lhs .!= Self(repeating: rhs)
  }

  @inlinable
  static func <(lhs: Self, rhs: Element) -> Mask {
    return lhs < Self(repeating: rhs)
  }
  
  @inlinable
  static func <=(lhs: Self, rhs: Element) -> Mask {
    return lhs <= Self(repeating: rhs)
  }
  
  @inlinable
  static func >(lhs: Self, rhs: Element) -> Mask {
    return lhs > Self(repeating: rhs)
  }
  
  @inlinable
  static func >=(lhs: Self, rhs: Element) -> Mask {
    return lhs >= Self(repeating: rhs)
  }
  
  @inlinable
  static func .==(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) .== rhs
  }
  
  @inlinable
  static func .!=(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) .!= rhs
  }
  
  @inlinable
  static func <(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) < rhs
  }
  
  @inlinable
  static func <=(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) <= rhs
  }
  
  @inlinable
  static func >(lhs: Element, rhs: Self) -> Mask {
    return Self(repeating: lhs) > rhs
  }
  
  @inlinable
  static func >=(lhs: Element, rhs: Self) -> Mask {
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
  static func .*(lhs: Self, rhs: Element) -> Self {
    return lhs .* Self(repeating: rhs)
  }
  
  @_transparent
  static func .*(lhs: Element, rhs: Self) -> Self {
    return rhs .* lhs
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
    lhs = lhs .* rhs
  }
  
  @_transparent
  static func *=(lhs: inout Self, rhs: Element) {
    lhs = lhs .* rhs
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
    for i in 0 ..< count { result[i] = self[i].squareRoot() }
    return result
  }
  
  @_transparent
  func rounded(_ rule: FloatingPointRoundingRule) -> Self {
    var result = self
    result.round(rule)
    return result
  }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: Range<Element>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in 0 ..< result.count {
      result[i] = Element.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: Range<Element>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Element>,
    using generator: inout T
  ) -> Self {
    var result = Self()
    for i in 0 ..< result.count {
      result[i] = Element.random(in: range, using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random(in range: ClosedRange<Element>) -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(in: range, using: &g)
  }
}


