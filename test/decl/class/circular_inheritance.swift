// RUN: %target-parse-verify-swift

class C : B { } // expected-error{{circular class inheritance 'C' -> 'B' -> 'A' -> 'C'}}
class B : A { } // expected-note{{class 'B' declared here}}
// expected-error@-1{{type 'B' does not conform to protocol 'AnyObject'}}
class A : C { } // expected-note{{class 'A' declared here}}
// expected-error@-1{{type 'A' does not conform to protocol 'AnyObject'}}

class TrivialCycle : TrivialCycle {} // expected-error{{circular class inheritance TrivialCycle}}
protocol P : P {} // expected-error{{circular protocol inheritance P}}
