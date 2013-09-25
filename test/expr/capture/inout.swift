// RUN: %swift -parse %s -verify

// A [inout] parameter cannot be captured.
func foo(x : [inout] Int) {
  func bar() -> Int {
    return x
  }
}
