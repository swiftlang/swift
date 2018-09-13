public protocol SIMDIntegerVector : SIMDVector
                              where Element: FixedWidthInteger {
  
  init(bitMaskFrom predicate: Predicate)
  /*
  var leadingZeroBitCount: Self { get }
  
  var trailingZeroBitCount: Self { get }
  
  var nonzeroBitCount: Self { get }
  
  var byteSwapped: Self { get }
  
  var bigEndian: Self { get }
  
  var littleEndian: Self { get }
   */
  
  static func <(lhs: Self, rhs: Self) -> Predicate
  
  static func <=(lhs: Self, rhs: Self) -> Predicate
  
  static func >(lhs: Self, rhs: Self) -> Predicate
  
  static func >=(lhs: Self, rhs: Self) -> Predicate
  
  static prefix func ~(rhs: Self) -> Self
  
  static func ^(lhs: Self, rhs: Self) -> Self
  
  static func &(lhs: Self, rhs: Self) -> Self
  
  static func |(lhs: Self, rhs: Self) -> Self
  
  static func &>>(lhs: Self, rhs: Self) -> Self
  
  static func &<<(lhs: Self, rhs: Self) -> Self
  
  static func &+(lhs: Self, rhs: Self) -> Self
  
  static func &-(lhs: Self, rhs: Self) -> Self
  
  static func &*(lhs: Self, rhs: Self) -> Self
  
  static func /(lhs: Self, rhs: Self) -> Self
  
  static func %(lhs: Self, rhs: Self) -> Self
}

// MARK: Defaulted requirements and extensions
public extension SIMDIntegerVector {

  /// A vector where the value in each lane is selected from the corresponding
  /// lane of `self` (if that lane of `predicate` is `false`) or `other` (if
  /// that lane of `predicate` is `true`).
  ///
  /// For example, suppose we want to replace any negative values in a vector
  /// with zeros. We would do the following:
  /// ~~~~
  /// let x: Int32.Vector8(0, 2, -1, 7, -.min, -2, 1, 3)
  /// let y = x.replacing(with: 0, where: x < 0) // 0, 2, 0, 7, 0, 0, 1, 3
  @_transparent
  func replacing(with other: Self, where predicate: Predicate) -> Self {
    return replacingBits(with: other, where: Self(bitMaskFrom: predicate))
  }
  
  @_transparent
  func replacingBits(with other: Self, where mask: Self) -> Self {
    return self & ~mask | other & mask
  }
  
  // MARK: Comparison operators
  @_transparent
  static func < <Other>(lhs: Self, rhs: Other) -> Predicate
  where Other : BinaryInteger {
    guard rhs >= Element.min else { return Predicate(repeating: false) }
    guard rhs <= Element.max else { return Predicate(repeating: true) }
    return lhs < Self(repeating: Self.Element(rhs))
  }
  
  @_transparent
  static func <= <Other>(lhs: Self, rhs: Other) -> Predicate
  where Other : BinaryInteger {
    guard rhs >= Element.min else { return Predicate(repeating: false) }
    guard rhs <= Element.max else { return Predicate(repeating: true) }
    return lhs <= Self(repeating: Self.Element(rhs))
  }
  
  @_transparent
  static func > <Other>(lhs: Self, rhs: Other) -> Predicate
  where Other : BinaryInteger {
    return !(lhs <= rhs)
  }
  
  @_transparent
  static func >= <Other>(lhs: Self, rhs: Other) -> Predicate
  where Other : BinaryInteger {
    return !(lhs < rhs)
  }
  
  @_transparent
  static func < <Other>(lhs: Other, rhs: Self) -> Predicate
  where Other : BinaryInteger {
    return rhs > lhs
  }
  
  @_transparent
  static func <= <Other>(lhs: Other, rhs: Self) -> Predicate
  where Other : BinaryInteger {
    return rhs >= lhs
  }
  
  @_transparent
  static func > <Other>(lhs: Other, rhs: Self) -> Predicate
  where Other : BinaryInteger {
    return rhs < lhs
  }
  
  @_transparent
  static func >= <Other>(lhs: Other, rhs: Self) -> Predicate
  where Other : BinaryInteger {
    return rhs <= rhs
  }
  
  // MARK: Bitwise operators
  @_transparent
  static prefix func ~(rhs: Self) -> Self {
    return ~0 ^ rhs
  }
  
  @_transparent
  static func ^(lhs: Self, rhs: Element) -> Self {
    return lhs ^ Self(repeating: rhs)
  }
  
  @_transparent
  static func ^(lhs: Element, rhs: Self) -> Self {
    return rhs ^ lhs
  }
  
  @_transparent
  static func &(lhs: Self, rhs: Element) -> Self {
    return lhs & Self(repeating: rhs)
  }
  
  @_transparent
  static func &(lhs: Element, rhs: Self) -> Self {
    return rhs & lhs
  }
  
  @_transparent
  static func |(lhs: Self, rhs: Element) -> Self {
    return lhs | Self(repeating: rhs)
  }
  
  @_transparent
  static func |(lhs: Element, rhs: Self) -> Self {
    return rhs | lhs
  }
  
  @inlinable
  static func >>(lhs: Self, rhs: Self) -> Self {
    let limit = Element(Element.bitWidth - 1)
    if Element.isSigned {
      let negated = 0 &- rhs
      let left = (lhs &<< negated).replacing(with: 0, where: negated > limit)
      let right = lhs &>> Swift.min(rhs, limit)
      return left.replacing(with: right, where: rhs >= 0)
    } else {
      let right = lhs &>> rhs
      return right.replacing(with: 0, where: rhs > limit)
    }
  }
  
  @inlinable
  static func <<(lhs: Self, rhs: Self) -> Self {
    if Element.isSigned {
      var left = (lhs &<< rhs)
      left.replace(with: 0, where: rhs >= Element.bitWidth)
      let right = lhs &>> Swift.min(0 &- rhs, Element(Element.bitWidth - 1))
      return left.replacing(with: right, where: rhs <= 0)
    } else {
      let right = lhs &>> rhs
      return right.replacing(with: 0, where: rhs >= Element.bitWidth)
    }
  }
  
  @_transparent
  static func &>> <Count>(lhs: Self, rhs: Count) -> Self
  where Count: BinaryInteger {
    return lhs &>> Self(repeating: Element(truncatingIfNeeded: rhs))
  }
  
  @_transparent
  static func &>>(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &>> rhs
  }
  
  @_transparent
  static func &<< <Count>(lhs: Self, rhs: Count) -> Self
  where Count: BinaryInteger {
    return lhs &<< Self(repeating: Element(truncatingIfNeeded: rhs))
  }
  
  @_transparent
  static func &<<(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &<< rhs
  }
  
  // MARK: In-place bitwise operators
  @_transparent
  static func ^=(lhs: inout Self, rhs: Self) {
    lhs = lhs ^ rhs
  }
  
  @_transparent
  static func ^=(lhs: inout Self, rhs: Element) {
    lhs = lhs ^ rhs
  }
  
  @_transparent
  static func &=(lhs: inout Self, rhs: Self) {
    lhs = lhs & rhs
  }
  
  @_transparent
  static func &=(lhs: inout Self, rhs: Element) {
    lhs = lhs & rhs
  }
  
  @_transparent
  static func |=(lhs: inout Self, rhs: Self) {
    lhs = lhs | rhs
  }
  
  @_transparent
  static func |=(lhs: inout Self, rhs: Element) {
    lhs = lhs | rhs
  }
  
  @_transparent
  static func &>>=(lhs: inout Self, rhs: Self) {
    lhs = lhs &>> rhs
  }
  
  @_transparent
  static func &>>= <Count>(lhs: inout Self, rhs: Count)
    where Count: BinaryInteger {
    lhs = lhs &>> rhs
  }
  
  @_transparent
  static func &<<=(lhs: inout Self, rhs: Self) {
    lhs = lhs &<< rhs
  }
  
  @_transparent
  static func &<<= <Count>(lhs: inout Self, rhs: Count)
    where Count: BinaryInteger {
    lhs = lhs &<< rhs
  }
  
  // MARK: Arithmetic operators
  @_transparent
  static func &+(lhs: Self, rhs: Element) -> Self {
    return lhs &+ Self(repeating: rhs)
  }
  
  @_transparent
  static func &+(lhs: Element, rhs: Self) -> Self {
    return rhs &+ lhs
  }
  
  @_transparent
  static func &-(lhs: Self, rhs: Element) -> Self {
    return lhs &- Self(repeating: rhs)
  }
  
  @_transparent
  static func &-(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &- rhs
  }
  
  @_transparent
  static func &*(lhs: Self, rhs: Element) -> Self {
    return lhs &* Self(repeating: rhs)
  }
  
  @_transparent
  static func &*(lhs: Element, rhs: Self) -> Self {
    return rhs &* lhs
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
  static func &+=(lhs: inout Self, rhs: Self) {
    lhs = lhs &+ rhs
  }
  
  @_transparent
  static func &+=(lhs: inout Self, rhs: Element) {
    lhs = lhs &+ rhs
  }
  
  @_transparent
  static func &-=(lhs: inout Self, rhs: Self) {
    lhs = lhs &- rhs
  }
  
  @_transparent
  static func &-=(lhs: inout Self, rhs: Element) {
    lhs = lhs &- rhs
  }
  
  @_transparent
  static func &*=(lhs: inout Self, rhs: Self) {
    lhs = lhs &* rhs
  }
  
  @_transparent
  static func &*=(lhs: inout Self, rhs: Element) {
    lhs = lhs &* rhs
  }
  
  @_transparent
  static func /=(lhs: inout Self, rhs: Self) {
    lhs = lhs / rhs
  }
  
  @_transparent
  static func /=(lhs: inout Self, rhs: Element) {
    lhs = lhs / rhs
  }
}

// MARK: Free functions implemented on the SIMDIntegerVector
@inlinable
public func min<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDIntegerVector {
  return lhs.replacing(with: rhs, where: rhs < lhs)
}

@inlinable
public func min<V>(_ lhs: V, _ rhs: V.Element) -> V where V: SIMDIntegerVector {
  return min(lhs, V(repeating: rhs))
}

@inlinable
public func min<V>(_ lhs: V.Element, _ rhs: V) -> V where V: SIMDIntegerVector {
  return min(V(repeating: lhs), rhs)
}

@inlinable
public func max<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDIntegerVector {
  return lhs.replacing(with: rhs, where: rhs >= lhs)
}

@inlinable
public func max<V>(_ lhs: V, _ rhs: V.Element) -> V where V: SIMDIntegerVector {
  return max(lhs, V(repeating: rhs))
}

@inlinable
public func max<V>(_ lhs: V.Element, _ rhs: V) -> V where V: SIMDIntegerVector {
  return max(V(repeating: lhs), rhs)
}

@inlinable
public func clamp<V>(_ value: V, min lowerBound: V, max upperBound: V) -> V where V: SIMDIntegerVector {
  return min(max(value, lowerBound), upperBound)
}

@inlinable
public func clamp<V>(_ value: V, min lowerBound: V.Element, max upperBound: V.Element) -> V
where V: SIMDIntegerVector {
  return min(max(value, lowerBound), upperBound)
}
