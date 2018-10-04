// RUN: %target-typecheck-verify-swift

infix operator +++ : AdditionPrecedence
infix operator *** : MultiplicationPrecedence

protocol P {
  static func +++ (lhs: Self, rhs: Self) -> Self
}

extension P {
  static func +++ (lhs: Self, rhs: Self) -> Self {
    return lhs
  }
}

protocol Q {
  static func *** (lhs: Self, rhs: Self) -> Self
}

struct Y : Q {
  static func *** (lhs: Y, rhs: Y) -> Y {
    return rhs
  }
}

struct X : P, ExpressibleByIntegerLiteral, ExpressibleByStringLiteral
{
  typealias IntegerLiteralType = Int
  public init(integerLiteral value: IntegerLiteralType) {}

  typealias StringLiteralType = String
  public init(stringLiteral value: StringLiteralType) {}
}

// This overload is required in order to be able to typecheck the
// expression at the bottom.
extension X : Q {
  static func *** (lhs: X, rhs: X) -> X {
    return rhs
  }
}

extension Int : P {}
extension String : P {}

let _ = 1 +++ "hi" +++ 3 *** 4 +++ 5
