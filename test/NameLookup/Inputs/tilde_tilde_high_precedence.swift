// Part of operators.swift multi-file test.

infix operator ~~ : HighPrecedence
precedencegroup HighPrecedence {
  associativity: none
  higherThan: BitwiseShiftPrecedence
}

public func ~~(x: Int, y: Int) -> Bool {
  return x < y
}
