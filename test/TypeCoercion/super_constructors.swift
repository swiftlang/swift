// RUN: %swift %s -parse-as-library -verify

struct S {
  constructor() {
    super.constructor() // expected-error{{'super.constructor' can only be called in a class constructor}}
  }
}

class D : B {
  func foo() {
    super.constructor() // expected-error{{'super.constructor' cannot be called outside of a constructor}}
  }

  constructor(a:Int) {
    super.constructor()
  }

  /*
  constructor(b:Int) {
    super.constructor()
    super.constructor() // e/xpected-error{{'super.constructor' can only be called once per constructor}}
  }

  constructor(c:Int) {
    super.constructor()
    super.constructor(0) // e/xpected-error{{'super.constructor' can only be called once per constructor}}
  }

  constructor(d:Int) {
    (super.constructor(),
     super.constructor()) // e/xpected-error{{'super.constructor' can only be called once per constructor}}
  }

  constructor(e:Int) {
    super.constructor('x') // e/xpected-error{{xxx ambiguous}}
  }

  constructor(f:Int) {
    super.constructor(a='x')
  }

  constructor(g:Int) {
    super.constructor("aoeu") // e/xpected-error{{xxx no matching call}}
  }
  */
}

class B {
  var foo : (bar:Int)
  func bar() {}

  constructor() {
  }
  /*
  constructor(x:Int) {
  }

  constructor(a:Char) {
  }
  constructor(b:Char) {
  }

  constructor(z:Float) {
    super.constructor() // e/xpected-error{{'super.constructor' cannot be called in a root class constructor}}
  }
  */
}

