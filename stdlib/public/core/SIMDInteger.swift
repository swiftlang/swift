public protocol SIMDIntegerVector : SIMDVector
                    where Element : FixedWidthInteger {
  
  /// Creates a bitmask vector from `mask`.
  ///
  /// The value of the mask is all 1 bits (i.e. `-1` if the element type is
  /// signed, .max if it is unsigned) in each lane where `mask` is true,
  /// and all 0 bits in each lane where `mask` is false.
  init(bitMaskFrom mask: Mask)
  
  /// A vector where each element is the count of leading zero bits in the
  /// corresponding lane of this vector.
  ///
  /// If a lane of this vector is zero, the corresponding lane of the result
  /// has the value Element.bitWidth.
  var leadingZeroBitCount: Self { get }
  
  /// A vector where each element is the count of trailing zero bits in the
  /// corresponding lane of this vector.
  ///
  /// If a lane of this vector is zero, the corresponding lane of the result
  /// has the value Element.bitWidth.
  var trailingZeroBitCount: Self { get }
  
  /// A vector where each element is the count of non-zero bits in the
  /// corresponding lane of this vector.
  var nonzeroBitCount: Self { get }
  
  /// A representation of this vector with the byte order swapped for each
  /// element.
  ///
  /// The ordering of elements within the vector is unchanged.
  var elementBytesSwapped: Self { get }
  
  static func .<(lhs: Self, rhs: Self) -> Mask
  
  static func .<=(lhs: Self, rhs: Self) -> Mask
  
  static func .>(lhs: Self, rhs: Self) -> Mask
  
  static func .>=(lhs: Self, rhs: Self) -> Mask
  
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
  
  /// The zero vector.
  static var zero: Self { return Self() }
  
  // MARK: Comparison with any BinaryInteger scalar, handling cases where
  // the scalar is outside the range of representable values for the vector
  // element type correctly, as with heterogeneous scalar integer comparisons.
  @inlinable
  static func .== <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    guard rhs >= Element.min else { return Mask(repeating: false) }
    guard rhs <= Element.max else { return Mask(repeating: false) }
    return lhs .== Self(repeating: Self.Element(truncatingIfNeeded: rhs))
  }
  
  @inlinable
  static func .!= <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    guard rhs >= Element.min else { return Mask(repeating: true) }
    guard rhs <= Element.max else { return Mask(repeating: true) }
    return lhs .!= Self(repeating: Self.Element(truncatingIfNeeded: rhs))
  }
  
  @inlinable
  static func .< <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    guard rhs >= Element.min else { return Mask(repeating: false) }
    guard rhs <= Element.max else { return Mask(repeating: true) }
    return lhs .< Self(repeating: Self.Element(truncatingIfNeeded: rhs))
  }
  
  @inlinable
  static func .<= <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    guard rhs >= Element.min else { return Mask(repeating: false) }
    guard rhs <= Element.max else { return Mask(repeating: true) }
    return lhs .<= Self(repeating: Self.Element(truncatingIfNeeded: rhs))
  }
  
  @inlinable
  static func .> <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    return !(lhs .<= rhs)
  }
  
  @inlinable
  static func .>= <Scalar>(lhs: Self, rhs: Scalar) -> Mask
  where Scalar : BinaryInteger {
    return !(lhs .< rhs)
  }
  
  @inlinable
  static func .== <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .== lhs
  }
  
  @inlinable
  static func .!= <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .!= lhs
  }
  
  @inlinable
  static func .< <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .> lhs
  }
  
  @inlinable
  static func .<= <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .>= lhs
  }
  
  @inlinable
  static func .> <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .< lhs
  }
  
  @inlinable
  static func .>= <Scalar>(lhs: Scalar, rhs: Self) -> Mask
  where Scalar : BinaryInteger {
    return rhs .<= lhs
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
  
  //  MARK: masking shifts with BinaryInteger counts
  @_transparent
  static func &>> <Count>(lhs: Self, rhs: Count) -> Self
  where Count: BinaryInteger {
    return lhs &>> Self(repeating: Element(truncatingIfNeeded: rhs))
  }
  
  @_transparent
  static func &<< <Count>(lhs: Self, rhs: Count) -> Self
  where Count: BinaryInteger {
    return lhs &<< Self(repeating: Element(truncatingIfNeeded: rhs))
  }
  
  //  MARK: masking shifts with scalar lhs
  //  These take Element for the LHS rather than an arbitrary BinaryInteger,
  //  because the semantics of integer scalars are that the lhs determines
  //  the type of a shift result, and we want to mirror that for vectors.
  @_transparent
  static func &>>(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &>> rhs
  }
  
  @_transparent
  static func &<<(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &<< rhs
  }
  
  //  MARK: Smart shifts
  @inlinable
  static func >>(lhs: Self, rhs: Self) -> Self {
    let limit = Element(Element.bitWidth - 1)
    if Element.isSigned {
      let negated = 0 &- rhs
      let left = (lhs &<< negated).replacing(with: 0, where: negated .> limit)
      let right = lhs &>> Swift.min(rhs, limit)
      return left.replacing(with: right, where: rhs .>= 0)
    } else {
      let right = lhs &>> rhs
      return right.replacing(with: 0, where: rhs .> limit)
    }
  }
  
  @inlinable
  static func <<(lhs: Self, rhs: Self) -> Self {
    let limit = Element(Element.bitWidth - 1)
    if Element.isSigned {
      let left = (lhs &<< rhs).replacing(with: 0, where: rhs .> limit)
      let right = lhs &>> Swift.min(0 &- rhs, limit)
      return left.replacing(with: right, where: rhs .<= 0)
    } else {
      let right = lhs &>> rhs
      return right.replacing(with: 0, where: rhs .> limit)
    }
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
  
  @_transparent
  static func %(lhs: Self, rhs: Element) -> Self {
    return lhs % Self(repeating: rhs)
  }
  
  @_transparent
  static func %(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) % rhs
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
  
  /// A vector where the value in each lane is selected from the corresponding
  /// lane of `self` (if that lane of `mask` is `false`) or `other` (if
  /// that lane of `mask` is `true`).
  @_transparent
  func replacing(with other: Self, where mask: Mask) -> Self {
    return replacingBits(with: other, where: Self(bitMaskFrom: mask))
  }
  
  /// A vector where each bit is selected from the corresponding bit of this
  /// vector or `other` depending on the value of the corresponding bit of
  /// `mask`.
  @_transparent
  func replacingBits(with other: Self, where mask: Self) -> Self {
    return self & ~mask | other & mask
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

// MARK: Free functions implemented on the SIMDIntegerVector
@inlinable
public func min<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDIntegerVector {
  return lhs.replacing(with: rhs, where: rhs .< lhs)
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
  return lhs.replacing(with: rhs, where: rhs .>= lhs)
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
public func clamp<V>(_ value: V, _ lowerBound: V, _ upperBound: V) -> V where V: SIMDIntegerVector {
  return min(max(value, lowerBound), upperBound)
}

@inlinable
public func clamp<V>(_ value: V, _ lowerBound: V.Element, _ upperBound: V.Element) -> V
where V: SIMDIntegerVector {
  return min(max(value, lowerBound), upperBound)
}
