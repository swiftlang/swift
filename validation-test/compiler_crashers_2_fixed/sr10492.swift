// RUN: not %target-swift-frontend -emit-silgen %s

struct S {
  func foo() -> Int! { return 0 }
}

let s = S()
let x: Int = (s.foo)()
