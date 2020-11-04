// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

async let x = 1 // okay

struct X {
  async let x = 1 // expected-error{{'async let' can only be used on local declarations}}
}

func testAsyncFunc() async {
  async let (z1, z2) = (2, 3)
  async let (_, _) = (2, 3)
  async let x2 = 1

  async var x = 17 // expected-error{{'async' can only be used with 'let' declarations}}{{9-12=let}}
  async let (_, _) = (1, 2), y2 = 7 // expected-error{{'async let' requires at least one named variable}}
  async let y: Int // expected-error{{'async let' binding requires an initializer expression}}
  _ = x
  _ = y
  _ = z1
  _ = z2
  _ = x2
  x = 1
  _ = y2
}
