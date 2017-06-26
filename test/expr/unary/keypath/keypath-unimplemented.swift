// RUN: %target-swift-frontend -typecheck -verify %s

struct A {
  subscript(x: Int) -> Int { return x }
  var c: C? = C()
}

class C {
  var i = 0
}

func unsupportedComponents() {
  _ = \A.[0] // expected-error{{key path support for subscript components is not implemented}}
}

// rdar://problem/32209039 - Improve diagnostic when unsupported tuple element references are used in key path literals
let _ = \(Int, String).0 // expected-error {{key path cannot reference tuple elements}}
let _ = \(a: Int, b: String).b // expected-error {{key path cannot reference tuple elements}}
