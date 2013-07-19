// RUN: %swift -parse -verify %s

protocol Runcible { func runce() }
protocol Fungible { func funge() }
protocol Ansible { func anse() }

protocol NeedsGenericMethods {
  func oneArgNoConstraints<T>(x:T) // expected-note {{protocol requires function 'oneArgNoConstraints' with type '<T> (x : T) -> ()'}}
  func oneArgWithConstraint<T:Runcible>(x:T) // expected-note {{protocol requires function 'oneArgWithConstraint' with type '<T : Runcible> (x : T) -> ()'}}
  func oneArgWithConstraints<T:protocol<Runcible, Fungible>>(x:T) // expected-note {{protocol requires function 'oneArgWithConstraints' with type '<T : protocol<Fungible, Runcible>> (x : T) -> ()'}}

  func twoArgsOneVar<T>(x:T, y:T) // expected-note {{protocol requires function 'twoArgsOneVar' with type '<T> (x : T, y : T) -> ()'}}
  func twoArgsTwoVars<T, U>(x:T, y:U) // expected-note {{protocol requires function 'twoArgsTwoVars' with type '<T, U> (x : T, y : U) -> ()'}}
}

class EqualConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x:U) {}
  func oneArgWithConstraint<U:Runcible>(x:U) {}
  func oneArgWithConstraints<U:protocol<Fungible, Runcible>>(x:U) {}

  func twoArgsOneVar<U>(x:U, y:U) {}
  func twoArgsTwoVars<V, U>(x:U, y:V) {}
}

class LooseConstraints : NeedsGenericMethods {
  func oneArgNoConstraints<U>(x:U) {}
  func oneArgWithConstraint<U>(x:U) {}
  func oneArgWithConstraints<U:Fungible>(x:U) {}

  func twoArgsOneVar<V, U>(x:U, y:V) {}
  func twoArgsTwoVars<V, U>(x:U, y:V) {}
}

class TooTightConstraints : NeedsGenericMethods { // expected-error{{type 'TooTightConstraints' does not conform to protocol 'NeedsGenericMethods'}}
  func oneArgNoConstraints<U:Runcible>(x:U) {} // expected-note{{candidate has non-matching type '<U : Runcible> (U) -> ()'}}
  func oneArgWithConstraint<U:Fungible>(x:U) {} // expected-note{{candidate has non-matching type '<U : Fungible> (U) -> ()'}}
  func oneArgWithConstraints<U:protocol<Runcible, Fungible, Ansible>>(x:U) {} // expected-note{{candidate has non-matching type '<U : protocol<Ansible, Fungible, Runcible>> (U) -> ()'}}

  func twoArgsOneVar<U:Runcible>(x:U) {} // expected-note{{candidate has non-matching type '<U : Runcible> (U) -> ()'}}
  func twoArgsTwoVars<U>(x:U, y:U) {} // expected-note{{candidate has non-matching type '<U> (U, U) -> ()'}}
}
