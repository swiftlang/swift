public protocol SIMDMask : SIMDVector
                     where Element == Bool, Mask == Self {  
  /// A mask vector with each lane is true where the corresponding
  /// lanes of both arguments are true, and false otherwise.
  static func &&(lhs: Self, rhs: Self) -> Self
  
  /// A mask vector with each lane is true where the corresponding
  /// lanes of either argument is true, and false otherwise.
  static func ||(lhs: Self, rhs: Self) -> Self
  
  func _all() -> Bool
  func _any() -> Bool
}

// Non-customizable extensions
public extension SIMDMask {
  @_transparent
  static prefix func !(rhs: Self) -> Self {
    return Self() .== rhs
  }
  
  @_transparent
  static func &&(lhs: Self, rhs: Bool) -> Self {
    return lhs && Self(repeating: rhs)
  }
  
  @_transparent
  static func &&(lhs: Bool, rhs: Self) -> Self {
    return rhs && lhs
  }
  
  @_transparent
  static func ||(lhs: Self, rhs: Bool) -> Self {
    return lhs || Self(repeating: rhs)
  }
  
  @_transparent
  static func ||(lhs: Bool, rhs: Self) -> Self {
    return rhs || lhs
  }
  
  @inlinable
  static func random<T: RandomNumberGenerator>(using generator: inout T) -> Self {
    var result = Self()
    for i in 0 ..< result.count {
      result[i] = Bool.random(using: &generator)
    }
    return result
  }
  
  @inlinable
  static func random() -> Self {
    var g = SystemRandomNumberGenerator()
    return Self.random(using: &g)
  }
}

// Defaulted requirements of SIMDVector
public extension SIMDMask {
  @_transparent
  func replacing(with other: Self, where mask: Self) -> Self {
    return self && !mask || other && mask
  }
}

// Free functions
@_transparent
public func all<P>(_ mask: P) -> Bool where P: SIMDMask {
  return mask._all()
}

@_transparent
public func any<P>(_ mask: P) -> Bool where P: SIMDMask {
  return mask._any()
}
