protocol NumericOperations : Comparable {
  static func add(lhs: Self, rhs: Self) -> (Self, Bool)
  static func sub(lhs: Self, rhs: Self) -> (Self, Bool)
  static func mul(lhs: Self, rhs: Self) -> (Self, Bool)
}

protocol SignedNumber {
  static func negate(rhs: Self) -> (Self, Bool)
  static func abs(rhs: Self) -> (Self, Bool)
  func isNegative() -> Bool
}

@prefix @transparent
func - <T : SignedNumber>(x: T) -> T {
  var tmp = T.negate(x)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}
