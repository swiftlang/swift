// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1

struct S {
  let a: Int
  let b: Int
  let c: Int
  let d: Int

  init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }
}

func foo(_ lhs: S, _ rhs: S) -> Int {
  return abs(lhs.a - rhs.a) + abs(lhs.b - rhs.b) + abs(lhs.c - rhs.c) + abs(lhs.d - rhs.d)
}
