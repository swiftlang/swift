operator infix %%% {
  associativity left
  precedence 200
}

func %%% (lhs: Int, rhs: Int) -> Int {
  return lhs + rhs
}
