// Part of operators.swift multi-file test.

infix operator ~~ {
  associativity none
  precedence 5
}

public func ~~(x: Int, y: Int) -> Bool {
  return x < y
}

infix operator ~~~ {
  associativity none
  precedence 5
}
