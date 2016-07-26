// Part of operators.swift multi-file test.

infix operator ~~ : LowPrecedence

precedencegroup LowPrecedence {
  associativity: none
  lowerThan: AssignmentPrecedence
}

public func ~~(x: Int, y: Int) -> Bool {
  return x < y
}

infix operator ~~~ : LowPrecedence
