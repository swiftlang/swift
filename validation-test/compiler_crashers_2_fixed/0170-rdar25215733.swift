// RUN: %target-typecheck-verify-swift

struct Foo {
  var x: Int = 0 // expected-note {{declared here}}
  struct x {}    // expected-note {{declared here}}

  init() {
    let a = x() // expected-error {{ambiguous use of 'x'}}
  }
}
