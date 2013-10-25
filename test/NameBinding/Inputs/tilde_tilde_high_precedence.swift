// Part of operators.swift multi-file test.

operator infix ~~ {
  associativity none
  precedence 200
}

func ~~(x: Int, y: Int) -> Bool {
  return x < y
}
