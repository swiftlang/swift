// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on

class Left // expected-error {{'Left' inherits from itself}} expected-note {{through reference here}}
    : Right.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class Right // expected-note {{through reference here}} expected-note{{class 'Right' declared here}}
  : Left.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class C : B { } // expected-error{{'C' inherits from itself}}
class B : A { } // expected-note{{class 'B' declared here}}
class A : C { } // expected-note{{class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{'TrivialCycle' inherits from itself}}
protocol P : P {} // expected-error {{protocol 'P' refines itself}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{'Automorphism' inherits from itself}}

let _ = A()

class Outer {
  class Inner : Outer {}
}

class Outer2 // expected-error {{'Outer2' inherits from itself}} expected-note  {{through reference here}}
    : Outer2.Inner { // expected-note {{through reference here}}

  class Inner {}
}

class Outer3 // expected-error {{'Outer3' inherits from itself}} expected-note  {{through reference here}}
    : Outer3.Inner<Int> { // expected-note {{through reference here}}
  class Inner<T> {}
}

protocol Initable {
  init()
}

protocol Shape : Circle {}

class Circle : Initable & Circle {}
// expected-error@-1 {{'Circle' inherits from itself}}
// expected-error@-2 {{initializer requirement 'init()' can only be satisfied by a 'required' initializer in non-final class 'Circle'}}

func crash() {
  _ = Circle()
}

class WithDesignatedInit : WithDesignatedInit {
  // expected-error@-1 {{'WithDesignatedInit' inherits from itself}}

  init(x: Int) {}
}
