// RUN: %target-parse-verify-swift -parse-as-library -enable-character-literals

struct S {
  init() {
    super.init() // expected-error{{'super' cannot be used outside of class members}}
  }
}

class D : B {
  func foo() {
    super.init() // expected-error{{'super.init' cannot be called outside of an initializer}}
  }

  init(a:Int) {
    super.init()
  }

  init(e:Int) {
    super.init('x') // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}}
  }

  init(f:Int) {
    super.init(a: "x")
  }

  init(g:Int) {
    super.init("aoeu") // expected-error{{}}
  }

  init(h:Int) {
    var y : B = super.init() // expected-error{{'()' is not convertible to 'B'}}
  }

  init(d:Double) {
    super.init()
  }
}

class B {
  var foo : Int
  func bar() {}

  init() {
  }

  init(x:Int) {
  }

  init(a:UnicodeScalar) {
  }
  init(b:UnicodeScalar) {
  }

  init(z:Float) {
    super.init() // expected-error{{'super' members cannot be referenced in a root class}}
  }
}

