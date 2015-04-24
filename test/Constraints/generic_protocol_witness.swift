// RUN: %target-parse-verify-swift

infix operator • { }

protocol Runcible { func runce() }
protocol Fungible { func funge() }
protocol Ansible { func anse() }

protocol NeedsGenericMethods {
  func oneArgNoConstraints<T>(x x : T) // expected-note {{protocol requires function 'oneArgNoConstraints(x:)' with type '<T> (x: T) -> ()'}}
  func oneArgWithConstraint<T : Runcible>(x x: T) // expected-note {{protocol requires function 'oneArgWithConstraint(x:)' with type '<T> (x: T) -> ()'}}
  func oneArgWithConstraints<T : protocol<Runcible, Fungible>>(x x: T) // expected-note {{protocol requires function 'oneArgWithConstraints(x:)' with type '<T> (x: T) -> ()'}}

  func twoArgsOneVar<T>(x x: T, y: T) // expected-note {{protocol requires function 'twoArgsOneVar(x:y:)' with type '<T> (x: T, y: T) -> ()'}}
  func twoArgsTwoVars<T, U>(x x: T, y: U) // expected-note {{protocol requires function 'twoArgsTwoVars(x:y:)' with type '<T, U> (x: T, y: U) -> ()'}}

  func •<T : Fungible>(x: Self, y: T) // expected-note {{protocol requires function '•' with type '<T> (TooTightConstraints, T) -> ()'}}
}

class EqualConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x x: U) {}
  func oneArgWithConstraint<U : Runcible>(x x: U) {}
  func oneArgWithConstraints<U : protocol<Fungible, Runcible>>(x x: U) {}

  func twoArgsOneVar<U>(x x: U, y: U) {}
  func twoArgsTwoVars<V, U>(x x: U, y: V) {}
}
func •<T : Fungible>(x: EqualConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T where T : Fungible> (EqualConstraints, T) -> ()'}}

class LooseConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x x: U) {}
  func oneArgWithConstraint<U>(x x: U) {}
  func oneArgWithConstraints<U : Fungible>(x x: U) {}

  func twoArgsOneVar<V, U>(x x: U, y: V) {}
  func twoArgsTwoVars<V, U>(x x: U, y: V) {}
}
func •<T>(x: LooseConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T> (LooseConstraints, T) -> ()'}}

class TooTightConstraints : NeedsGenericMethods { // expected-error{{type 'TooTightConstraints' does not conform to protocol 'NeedsGenericMethods'}}
  func oneArgNoConstraints<U : Runcible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}
  func oneArgWithConstraint<U : Fungible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}
  func oneArgWithConstraints<U : protocol<Runcible, Fungible, Ansible>>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}

  func twoArgsOneVar<U : Runcible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}
  func twoArgsTwoVars<U>(x x: U, y: U) {} // expected-note{{candidate has non-matching type '<U> (x: U, y: U) -> ()'}}
}
func •(x: TooTightConstraints, y: Int) {} // expected-note {{candidate has non-matching type '(TooTightConstraints, Int) -> ()'}}

// Regression test for a crash when resolving a reference to a generic method
// in a protocol.
protocol NeedsAGenericMethod {
  func method<T>(x: T)
}
func usesAGenericMethod<U : NeedsAGenericMethod>(x: U) {
  x.method(5)
}

struct L<T>: SequenceType {} // expected-error {{type 'L<T>' does not conform to protocol '_Sequence_Type'}} expected-error {{type 'L<T>' does not conform to protocol 'SequenceType'}} expected-error {{type 'L<T>' does not conform to protocol '_SequenceDefaultsType'}}

func z(x: L<Int>) {
  for xx in x {} // expected-error{{'L<Int>' does not have a member named 'Generator'}}
}
