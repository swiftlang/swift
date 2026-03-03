// RUN: %target-typecheck-verify-swift

class Left // expected-error {{'Left' inherits from itself}}
    : Right.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class Right // expected-note {{through class 'Right' declared here}}
  : Left.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class C : B { } // expected-error{{'C' inherits from itself}}
class B : A { } // expected-note{{through class 'B' declared here}}
class A : C { } // expected-note{{through class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{'TrivialCycle' inherits from itself}}
protocol P : P {} // expected-error {{protocol 'P' refines itself}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{'Automorphism' inherits from itself}}

let _ = A()

class Outer {
  class Inner : Outer {}
}

class Outer2 // expected-error {{'Outer2' inherits from itself}}
    : Outer2.Inner { // expected-note {{through reference here}}

  class Inner {}
}

class Outer3 // expected-error {{'Outer3' inherits from itself}}
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
