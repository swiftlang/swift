// RUN: %swift -parse %s -verify

// A [byref] parameter cannot be captured.
func foo(x : [byref] Int) {
  func bar() -> Int {
    return x // expected-error{{cannot capture [byref] parameter 'x'}}
  }
}
