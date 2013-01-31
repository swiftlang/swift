// RUN: %swift %s -constraint-checker -parse-as-library -verify

struct S {
  constructor() {
    super.constructor() // expected-error{{'super.constructor' cannot be called in a constructor for a non-class type}}
  }
}

class D : B {
  func foo() {
    super.constructor() // expected-error{{'super.constructor' cannot be called outside of a constructor}}
  }

  constructor(a:Int) {
    super.constructor()
  }

  constructor(e:Int) {
    super.constructor('x') // expected-error{{}}
  }

  constructor(f:Int) {
    super.constructor(a='x')
  }

  constructor(g:Int) {
    super.constructor("aoeu") // expected-error{{}}
  }
}

class B {
  var foo : (bar:Int)
  func bar() {}

  constructor() {
  }

  constructor(x:Int) {
  }

  constructor(a:Char) {
  }
  constructor(b:Char) {
  }

  constructor(z:Float) {
    super.constructor() // expected-error{{'super.constructor' cannot be called in a root class constructor}}
  }
}

