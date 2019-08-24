// RUN: %target-typecheck-verify-swift

// An inout parameter can be captured.
func foo(x: inout Int) {
  func bar() -> Int {
    return x
  }
}

// But not partially applied.
struct C {
  mutating func f(x: Int) {}
}

var c = C()
let x = c.f // expected-error{{partial application of 'mutating' method is not allowed}}
