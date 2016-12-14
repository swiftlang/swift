// RUN: %target-typecheck-verify-swift

protocol P {
    init()
}

class A : P { } // expected-error{{initializer requirement 'init()' can only be satisfied by a `required` initializer in non-final class 'A'}} 
// No further errors

class B : A {
    init(x : Int) {} // expected-note {{'init(x:)' declared here}}
}

class C : B { }

class D : B {
  init() {
    super.init() // expected-error{{missing argument for parameter 'x' in call}}
  }
}

