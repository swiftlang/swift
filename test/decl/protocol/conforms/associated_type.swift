// RUN: %target-parse-verify-swift

class C { }

protocol P { // expected-note{{requirement specified as 'Self.AssocP' : 'C' [with Self = X]}}
  associatedtype AssocP : C
}

struct X : P { // expected-error{{'P' requires that 'X.AssocP' (aka 'Int') inherit from 'C'}}
  typealias AssocP = Int
}
