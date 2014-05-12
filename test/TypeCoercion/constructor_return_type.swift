// RUN: %swift %s -verify

struct S {
  init(a:Bool) {
    return
  }

  init(b:Bool) {
    return 1 // expected-error {{cannot convert the expression's type 'Int' to type '()'}}
  }
}

