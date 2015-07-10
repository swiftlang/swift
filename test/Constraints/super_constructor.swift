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
    super.init('x') // expected-error{{expression does not conform to type 'CharacterLiteralConvertible'}}
  }

  init(f:Int) {
    super.init(a: "x")
  }

  init(g:Int) {
    super.init("aoeu") // expected-error{{cannot invoke 'B.init' with an argument list of type '(String)'}}
    // expected-note @-1 {{overloads for 'B.init' exist with these partially matching parameter lists: (x: Int), (a: UnicodeScalar), (b: UnicodeScalar), (z: Float)}}
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

