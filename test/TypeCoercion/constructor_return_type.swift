// RUN: %swift %s -verify

struct S {
  init(a:Bool) {
    return
  }

  init(b:Bool) {
    return 1 // expected-error {{type '()' does not conform to protocol 'IntegerLiteralConvertible'}}
  }
}

