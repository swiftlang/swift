// RUN: %target-typecheck-verify-swift

struct S: Hashable {
  let e: E?
}

enum E: Hashable {
  case foo
  case bar
}

let a = S(e: .foo)
let b = S(e: .bar)

_ = a == b
