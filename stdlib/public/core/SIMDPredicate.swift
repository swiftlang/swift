public protocol SIMDPredicate : SIMDVector
                          where Element == Bool, Predicate == Self {
  // Note: We use the bitwise & and | operators here instead of the && and ||
  // operators. Semantically, && and || would be better in some sense, except
  // for possible confusion about them not short-circuiting (it doesn't even
  // make sense to do that lanewise). The main problem, though, with && and ||
  // is that using them here makes the stdlib fail to typecheck (which is
  // a bug, but one that I don't have an easy workaround for).
  //
  // A much smaller issue is that there's no ^^ operator (you use != instead).
  
  /// A predicate vector with each lane is true where the corresponding
  /// lanes of the two arguments are different, and false otherwise.
  static func ^(lhs: Self, rhs: Self) -> Self
  
  /// A predicate vector with each lane is true where the corresponding
  /// lanes of both arguments are true, and false otherwise.
  static func &(lhs: Self, rhs: Self) -> Self
  
  /// A predicate vector with each lane is true where the corresponding
  /// lanes of either argument is true, and false otherwise.
  static func |(lhs: Self, rhs: Self) -> Self
}

// Non-customizable extensions
public extension SIMDPredicate {
  @_transparent
  static func ^(lhs: Self, rhs: Bool) -> Self {
    return lhs ^ Self(repeating: rhs)
  }
  
  @_transparent
  static func ^(lhs: Bool, rhs: Self) -> Self {
    return rhs ^ lhs
  }
  
  @_transparent
  static func &(lhs: Self, rhs: Bool) -> Self {
    return lhs & Self(repeating: rhs)
  }
  
  @_transparent
  static func &(lhs: Bool, rhs: Self) -> Self {
    return rhs & lhs
  }
  
  @_transparent
  static func |(lhs: Self, rhs: Bool) -> Self {
    return lhs | Self(repeating: rhs)
  }
  
  @_transparent
  static func |(lhs: Bool, rhs: Self) -> Self {
    return rhs | lhs
  }
}

// Defaulted requirements of SIMDVector
public extension SIMDPredicate {
  @_transparent
  func replacing(with other: Self, where predicate: Self) -> Self {
    return self & !predicate | other & predicate
  }
}

// Defaulted requirements of SIMDPredicate
public extension SIMDPredicate {
  @_transparent
  static prefix func !(rhs: Self) -> Self {
    return Self() == rhs
  }
}

// Free functions
@_transparent
public func all<P>(_ predicate: P) -> Bool where P: SIMDPredicate {
  return predicate.reduce(true) { $0 && $1 }
}

@_transparent
public func any<P>(_ predicate: P) -> Bool where P: SIMDPredicate {
  return predicate.reduce(false) { $0 || $1 }
}
