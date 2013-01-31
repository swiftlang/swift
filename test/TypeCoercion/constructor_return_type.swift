// RUN: %swift %s -verify

struct S {
  constructor(a:Bool) {
    return
  }

  constructor(b:Bool) {
    return 1 // expected-error{{}}
  }
}

