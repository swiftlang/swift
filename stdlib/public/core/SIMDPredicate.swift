public protocol SIMDPredicate : SIMDVector
                                /*ExpressibleByBooleanLiteral*/
                          where Element == Bool, Predicate == Self {
  
  static prefix func !(rhs: Self) -> Self
  
  static func &&(lhs: Self, rhs: Self) -> Self
  
  static func ||(lhs: Self, rhs: Self) -> Self
  
  static func all(_ predicate: Self) -> Bool
  
  static func any(_ predicate: Self) -> Bool
  
  static func first(where predicate: Self) -> Int?
  
  static func count(where predicate: Self) -> Int
}

// Defaulted requirements of SIMDVector
public extension SIMDPredicate {
  
  /*
  @_transparent
  init(booleanLiteral: Element) {
    self.init(repeating: booleanLiteral)
  }
  */
  
  @_transparent
  func replacing(with other: Self, where predicate: Self) -> Self {
    return self && !predicate || other && predicate
  }
}

// Defaulted requirements of SIMDPredicate
public extension SIMDPredicate {
  
  @_transparent
  static prefix func !(rhs: Self) -> Self {
    return Self() == rhs
  }
  
  @_transparent
  static func all(_ predicate: Self) -> Bool {
    return predicate.reduce(true, { $0 && $1 })
  }
  
  @_transparent
  static func any(_ predicate: Self) -> Bool {
    return predicate.reduce(false, { $0 || $1 })
  }
  
  @_transparent
  static func first(where predicate: Self) -> Int? {
    for i in predicate.indices {
      if predicate[i] { return i }
    }
    return nil
  }
  
  @_transparent
  static func count(where predicate: Self) -> Int {
    return predicate.reduce(0, { $0 + ($1 ? 1 : 0) })
  }
}

// Free functions
@_transparent
public func all<P>(_ predicate: P) -> Bool where P: SIMDPredicate {
  return P.all(predicate)
}

@_transparent
public func any<P>(_ predicate: P) -> Bool where P: SIMDPredicate {
  return P.any(predicate)
}

@_transparent
public func first<P>(where predicate: P) -> Int? where P: SIMDPredicate {
  return P.first(where: predicate)
}

@_transparent
public func count<P>(where predicate: P) -> Int where P: SIMDPredicate {
  return P.count(where: predicate)
}
