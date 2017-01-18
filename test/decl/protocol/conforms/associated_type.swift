// RUN: %target-typecheck-verify-swift

class C { }

protocol P {
  associatedtype AssocP : C // expected-note{{protocol requires nested type 'AssocP'; do you want to add it?}}
}

struct X : P { // expected-error{{type 'X' does not conform to protocol 'P'}}
  typealias AssocP = Int // expected-note{{possibly intended match 'X.AssocP' (aka 'Int') does not inherit from 'C'}}
}
