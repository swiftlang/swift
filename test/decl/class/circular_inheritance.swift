// RUN: %target-parse-verify-swift

class C : B { } // expected-error{{circular class inheritance 'C' -> 'B' -> 'A' -> 'C'}}
class B : A { } // expected-note{{class 'B' declared here}}
class A : C { } // expected-note{{class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{circular class inheritance TrivialCycle}}
protocol P : P {} // expected-error{{circular protocol inheritance P}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{circular class inheritance Automorphism}}

let _ = A()

// This should probably be made to work, but for now test that it produces a crappy
// diagnostic instead of crashing

class Left : Right.Hand {
  class Hand {}
}

class Right : Left.Hand { // expected-error {{'Hand' is not a member type of 'Left'}}
  class Hand {}
}

class Outer {
  class Inner : Outer {}
}
