// RUN: %target-parse-verify-swift

infix operator • { }

protocol Runcible { func runce() }
protocol Fungible { func funge() }
protocol Ansible { func anse() }

protocol NeedsGenericMethods {
  func oneArgNoConstraints<T>(x x : T) // expected-note {{protocol requires function 'oneArgNoConstraints(x:)' with type '<T> (x: T) -> Void'}}
  func oneArgWithConstraint<T : Runcible>(x x: T) // expected-note {{protocol requires function 'oneArgWithConstraint(x:)' with type '<T> (x: T) -> Void'}}
  func oneArgWithConstraints<T : protocol<Runcible, Fungible>>(x x: T) // expected-note {{protocol requires function 'oneArgWithConstraints(x:)' with type '<T> (x: T) -> Void'}}

  func twoArgsOneVar<T>(x x: T, y: T) // expected-note {{protocol requires function 'twoArgsOneVar(x:y:)' with type '<T> (x: T, y: T) -> Void'}}
  func twoArgsTwoVars<T, U>(x x: T, y: U) // expected-note {{protocol requires function 'twoArgsTwoVars(x:y:)' with type '<T, U> (x: T, y: U) -> Void'}}

  func •<T : Fungible>(x: Self, y: T) // expected-note {{protocol requires function '•' with type '<T> (TooTightConstraints, T) -> Void'}}
}

class EqualConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x x: U) {}
  func oneArgWithConstraint<U : Runcible>(x x: U) {}
  func oneArgWithConstraints<U : protocol<Fungible, Runcible>>(x x: U) {}

  func twoArgsOneVar<U>(x x: U, y: U) {}
  func twoArgsTwoVars<V, U>(x x: U, y: V) {}
}
func •<T : Fungible>(x: EqualConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T where T : Fungible> (EqualConstraints, T) -> Void'}}

class LooseConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x x: U) {}
  func oneArgWithConstraint<U>(x x: U) {}
  func oneArgWithConstraints<U : Fungible>(x x: U) {}

  func twoArgsOneVar<V, U>(x x: U, y: V) {}
  func twoArgsTwoVars<V, U>(x x: U, y: V) {}
}
func •<T>(x: LooseConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T> (LooseConstraints, T) -> Void'}}

class TooTightConstraints : NeedsGenericMethods { // expected-error{{type 'TooTightConstraints' does not conform to protocol 'NeedsGenericMethods'}}
  func oneArgNoConstraints<U : Runcible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> Void'}}
  func oneArgWithConstraint<U : Fungible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> Void'}}
  func oneArgWithConstraints<U : protocol<Runcible, Fungible, Ansible>>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> Void'}}

  func twoArgsOneVar<U : Runcible>(x x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> Void'}}
  func twoArgsTwoVars<U>(x x: U, y: U) {} // expected-note{{candidate has non-matching type '<U> (x: U, y: U) -> Void'}}
}
func •(x: TooTightConstraints, y: Int) {} // expected-note {{candidate has non-matching type '(TooTightConstraints, Int) -> Void'}}

// Regression test for a crash when resolving a reference to a generic method
// in a protocol.
protocol NeedsAGenericMethod {
  func method<T>(x: T)
}
func usesAGenericMethod<U : NeedsAGenericMethod>(x: U) {
  x.method(5)
}

struct L<T>: SequenceType {} // expected-error {{type 'L<T>' does not conform to protocol 'SequenceType'}}

func z(x: L<Int>) {
  for xx in x {}
}
