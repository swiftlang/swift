// RUN: %target-parse-verify-swift

class C { }

protocol P { // expected-note{{requirement specified as '`Self`.AssocP' : 'C' [with Self = X]}}
  typealias AssocP : C
}

struct X : P { // expected-error{{'P' requires that 'AssocP' inherit from 'C'}}
  typealias AssocP = Int
}
