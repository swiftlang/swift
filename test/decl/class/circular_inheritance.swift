// RUN: %target-typecheck-verify-swift

class C : B { } // expected-error{{circular class inheritance 'C' -> 'B' -> 'A' -> 'C'}}
class B : A { } // expected-note{{class 'B' declared here}}
class A : C { } // expected-note{{class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{circular class inheritance TrivialCycle}}
protocol P : P {} // expected-error 2{{circular protocol inheritance P}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{circular class inheritance Automorphism}}

// FIXME: Useless error
let _ = A() // expected-error{{'A' cannot be constructed because it has no accessible initializers}}

// FIXME: Not technically a circular dependency.
class Left  // expected-error{{circular class inheritance 'Left'}}
    : Right.Hand { // expected-note{{through reference here}}
  class Hand {}
}

class Right // expected-note{{through reference here}}
  : Left.Hand { // expected-note{{through reference here}}
  class Hand {}
}

class Outer {
  class Inner : Outer {}
}

class Outer2 // expected-error{{circular class inheritance 'Outer2'}}
    : Outer2.Inner { // expected-note{{through reference here}}

  class Inner {}
}

class Outer3 // expected-error{{circular class inheritance 'Outer3'}}
    : Outer3.Inner<Int> { // expected-note{{through reference here}}
  class Inner<T> {}
}
