/*
 
public protocol SIMDIntegerVector : SIMDVector,
                                    ExpressibleByIntegerLiteral
                              where Element: FixedWidthInteger {
  
  init(bitMaskFrom predicate: Predicate)
  
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
  
  static func min(_ lhs: Self, _ rhs: Self) -> Self
  
  static func max(_ lhs: Self, _ rhs: Self) -> Self
}

// MARK: Defaulted requirements
public extension SIMDIntegerVector {
  
  @_transparent
  func replacing(with other: Self, where predicate: Predicate) -> Self {
    let mask = Self(bitMaskFrom: predicate)
    return self & ~mask | other & mask
  }
  
  @_transparent
  func replacingBits(with other: Self, where mask: Self) -> Self {
    return self & ~mask | other & mask
  }
  
  @_transparent
  init(integerLiteral: Element) {
    self.init(repeating: Element(integerLiteral))
  }
  
  @_transparent
  static prefix func ~(rhs: Self) -> Self {
    return Self(repeating: ~0) ^ rhs
  }
  
  @_transparent
  static func &*(lhs: Self, rhs: Element) -> Self {
    return lhs &* Self(repeating: rhs)
  }
  
  @_transparent
  static func &*(lhs: Element, rhs: Self) -> Self {
    return Self(repeating: lhs) &* rhs
  }
  
  @_transparent
  static func /(lhs: Self, rhs: Element) -> Self {
    return lhs / Self(repeating: rhs)
  }
  
  @_transparent
  static func &>>(lhs: Self, rhs: Element) -> Self {
    return lhs &>> Self(repeating: rhs)
  }
  
  @_transparent
  static func &<<(lhs: Self, rhs: Element) -> Self {
    return lhs &<< Self(repeating: rhs)
  }
  
  @_transparent
  static func min(_ lhs: Self, _ rhs: Self) -> Self {
    return lhs.replacing(with: rhs, where: rhs < lhs)
  }
  
  @_transparent
  static func max(_ lhs: Self, _ rhs: Self) -> Self {
    return rhs.replacing(with: lhs, where: rhs < lhs)
  }
}

// MARK: Free functions implemented on the SIMDIntegerVector
@_transparent
public func min<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDIntegerVector {
  return V.min(lhs, rhs)
}

@_transparent
public func max<V>(_ lhs: V, _ rhs: V) -> V where V: SIMDIntegerVector {
  return V.max(lhs, rhs)
}

@_transparent
public func clamp<V>(_ value: V, min: V, max: V) -> V where V: SIMDIntegerVector {
  return V.min(V.max(value, min), max)
}

 */
