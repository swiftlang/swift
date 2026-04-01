// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000

func f1() {
  print((0 + 0) + abs(0) | (true ? 0 + 0 : 0 + 0) + 0)
  // expected-error@-1 {{reasonable time}}
}

func f2() {
  print((0 &+ 0) &+ abs(0) | (true ? 0 + 0 : 0 &+ 0) + 0)
  // expected-error@-1 {{reasonable time}}
}

func f3() {
  print((((((0 + 0) + max(0, (0 << 0))) | (true ? (!(true) ? (0 + 0) : (0 + 0)) : 0)) + 0) + 0))
  // expected-error@-1 {{reasonable time}}
}

func f4() {
  print((((((0 &+ 0) &+ max(0, (0 << 0))) | (true ? (!(true) ? (0 + 0) : (0 &+ 0)) : 0)) + 0) &+ 0))
  // expected-error@-1 {{reasonable time}}
}
