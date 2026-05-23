// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

enum E {
  case x
  case y(Bool)
}

func f(_: String, _: [E]) {}

func g(_: String) -> [E] {
  return []
}

func test() {
  f("", [.x] + g("") + [.x, .x] + g("") + [.x, .x, .x, .x] + g("") + [.x, .x] + g("") + [.x, .y(true), .x, .x])
}
