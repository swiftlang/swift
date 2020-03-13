// RUN: %target-typecheck-verify-swift

struct Foo {
  var bar: Int
}

let f = Foo(bar: nil!) // expected-error {{'nil' literal cannot be force unwrapped}}
