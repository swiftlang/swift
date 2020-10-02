// RUN: %target-typecheck-verify-swift -parse-as-library 

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

  init(f:Int) {
    super.init(a: "x")
  }

  init(g:Int) {
    super.init("aoeu") // expected-error{{no exact matches in call to initializer}}
  }

  init(h:Int) {
    var _ : B = super.init() // expected-error{{cannot convert value of type '()' to specified type 'B'}}
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

  init(x:Int) { // expected-note{{candidate expects value of type 'Int' for parameter #1}}
  }

  init(a:UnicodeScalar) { // expected-note {{candidate expects value of type 'UnicodeScalar' (aka 'Unicode.Scalar') for parameter #1}}
  }
  init(b:UnicodeScalar) { // expected-note {{candidate expects value of type 'UnicodeScalar' (aka 'Unicode.Scalar') for parameter #1}}
  }

  init(z:Float) { // expected-note{{candidate expects value of type 'Float' for parameter #1}}
    super.init() // expected-error{{'super' members cannot be referenced in a root class}}
  }
}

// SR-2484: Bad diagnostic for incorrectly calling private init
class SR_2484 {
  private init() {} // expected-note {{'init()' declared here}}
  private init(a: Int) {}
}

class Impl_2484 : SR_2484 {
  init() {
    super.init() // expected-error {{'SR_2484' initializer is inaccessible due to 'private' protection level}}
  }
}

class A_Priv<T> {
  private init(_ foo: T) {}
}

class B_Override<U> : A_Priv<[U]> {
  init(_ foo: [U]) { fatalError() } // Ok, because effectively overrides init from parent which is invisible
}
