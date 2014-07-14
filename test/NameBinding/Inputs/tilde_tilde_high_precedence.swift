// Part of operators.swift multi-file test.

infix operator ~~ {
  associativity none
  precedence 200
}

public func ~~(x: Int, y: Int) -> Bool {
  return x < y
}
