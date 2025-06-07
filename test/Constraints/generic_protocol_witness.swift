// RUN: %target-typecheck-verify-swift

infix operator •

protocol Runcible { func runce() }
protocol Fungible { func funge() }
protocol Ansible { func anse() }

protocol NeedsGenericMethods {
  func oneArgNoConstraints<T>(x : T) // expected-note {{protocol requires function 'oneArgNoConstraints(x:)' with type '<T> (x: T) -> ()'}}
  func oneArgWithConstraint<T : Runcible>(x: T) // expected-note {{protocol requires function 'oneArgWithConstraint(x:)' with type '<T> (x: T) -> ()'}}
  func oneArgWithConstraints<T : Runcible & Fungible>(x: T) // expected-note {{protocol requires function 'oneArgWithConstraints(x:)' with type '<T> (x: T) -> ()'}}

  func twoArgsOneVar<T>(x: T, y: T) // expected-note {{protocol requires function 'twoArgsOneVar(x:y:)' with type '<T> (x: T, y: T) -> ()'}}
  func twoArgsTwoVars<T, U>(x: T, y: U) // expected-note {{protocol requires function 'twoArgsTwoVars(x:y:)' with type '<T, U> (x: T, y: U) -> ()'}}

  static func •<T : Fungible>(x: Self, y: T) // expected-note {{protocol requires function '•' with type '<T> (TooTightConstraints, T) -> ()'}}
}

class EqualConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x: U) {}
  func oneArgWithConstraint<U : Runcible>(x: U) {}
  func oneArgWithConstraints<U : Fungible & Runcible>(x: U) {}

  func twoArgsOneVar<U>(x: U, y: U) {}
  func twoArgsTwoVars<V, U>(x: U, y: V) {}
}
func •<T : Fungible>(x: EqualConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T> (EqualConstraints, T) -> ()'}}

class LooseConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x: U) {}
  func oneArgWithConstraint<U>(x: U) {}
  func oneArgWithConstraints<U : Fungible>(x: U) {}

  func twoArgsOneVar<V, U>(x: U, y: V) {}
  func twoArgsTwoVars<V, U>(x: U, y: V) {}
}
func •<T>(_ x: LooseConstraints, y: T) {} // expected-note {{candidate has non-matching type '<T> (LooseConstraints, T) -> ()'}}

class TooTightConstraints : NeedsGenericMethods { // expected-error{{type 'TooTightConstraints' does not conform to protocol 'NeedsGenericMethods'}} expected-note {{add stubs for conformance}}
  func oneArgNoConstraints<U : Runcible>(x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}
  func oneArgWithConstraint<U : Fungible>(x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}
  func oneArgWithConstraints<U : Runcible & Fungible & Ansible>(x: U) {} // expected-note{{candidate has non-matching type '<U> (x: U) -> ()'}}

  func twoArgsOneVar<U : Runcible>(x: U) {}
  func twoArgsTwoVars<U>(x: U, y: U) {} // expected-note{{candidate has non-matching type '<U> (x: U, y: U) -> ()'}}
}
func •(_ x: TooTightConstraints, y: Int) {} // expected-note {{candidate has non-matching type '(TooTightConstraints, Int) -> ()'}}

// Regression test for a crash when resolving a reference to a generic method
// in a protocol.
protocol NeedsAGenericMethod {
  func method<T>(_ x: T)
}
func usesAGenericMethod<U : NeedsAGenericMethod>(_ x: U) {
  x.method(5)
}

struct L<T>: Sequence {} // expected-error {{type 'L<T>' does not conform to protocol 'Sequence'}}

func z(_ x: L<Int>) {
  for xx in x {}
}
