public protocol SIMDPredicate : SIMDVector
                          where Element == Bool, Predicate == Self {
  // Note: We use the bitwise & and | operators here instead of the && and ||
  // operators. Semantically, && and || would be better in some sense, except
  // for possible confusion about them not short-circuiting (it doesn't even
  // make sense to do that lanewise). The real problem with && and || is that
  // using them here makes the stdlib fail to typecheck.
  //
  // A much smaller issue is that there's no ^^ operator (you use != instead).
  static func ^(lhs: Self, rhs: Self) -> Self
  static func &(lhs: Self, rhs: Self) -> Self
  static func |(lhs: Self, rhs: Self) -> Self
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
