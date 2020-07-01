// RUN: %target-typecheck-verify-swift -swift-version 4

class C { }

protocol P {
  associatedtype AssocP : C // expected-note{{protocol requires nested type 'AssocP'; do you want to add it?}}
  associatedtype AssocA : AnyObject // expected-note{{protocol requires nested type 'AssocA'; do you want to add it?}}
}

struct X : P { // expected-error{{type 'X' does not conform to protocol 'P'}}
  typealias AssocP = Int // expected-note{{possibly intended match 'X.AssocP' (aka 'Int') does not inherit from 'C'}}
  typealias AssocA = Int // expected-note{{possibly intended match 'X.AssocA' (aka 'Int') does not conform to 'AnyObject'}}
}

// SR-5166
protocol FooType {
    associatedtype BarType

    func foo(bar: BarType)
    func foo(action: (BarType) -> Void)
}

protocol Bar {}

class Foo: FooType {
    typealias BarType = Bar

    func foo(bar: Bar) {
    }

    func foo(action: (Bar) -> Void) {
    }
}

// rdar://problem/35297911: noescape function types
protocol P1 {
  associatedtype A

  func f(_: A)
}

struct X1a : P1 {
  func f(_: @escaping (Int) -> Int) { }
}

struct X1b : P1 {
  typealias A = (Int) -> Int

  func f(_: @escaping (Int) -> Int) { }
}

struct X1c : P1 {
  typealias A = (Int) -> Int

  func f(_: (Int) -> Int) { }
}

struct X1d : P1 {
  func f(_: (Int) -> Int) { }
}

protocol P2 {
  func f(_: (Int) -> Int) // expected-note{{protocol requires function 'f' with type '((Int) -> Int) -> ()'; do you want to add a stub?}}
}

struct X2a : P2 {
  func f(_: (Int) -> Int) { }
}

struct X2b : P2 { // expected-error{{type 'X2b' does not conform to protocol 'P2'}}
  func f(_: @escaping (Int) -> Int) { } // expected-note{{candidate has non-matching type '(@escaping (Int) -> Int) -> ()'}}
}

// SR-12707

class SR_12707_C<T> {}

// Regular type witnesses
protocol SR_12707_P1 {
  associatedtype A
  associatedtype B: SR_12707_C<(A, Self)> // expected-note {{'B' declared here}}
}
struct SR_12707_Conform_P1: SR_12707_P1 {
  typealias A = Never
  typealias B = SR_12707_C<(A, SR_12707_Conform_P1)>
}

// Type witness in protocol extension
protocol SR_12707_P2: SR_12707_P1 {}
extension SR_12707_P2 {
  typealias B = SR_12707_C<(A, Self)> // expected-warning {{typealias overriding associated type 'B' from protocol 'SR_12707_P1' is better expressed as same-type constraint on the protocol}}
}
struct SR_12707_Conform_P2: SR_12707_P2 {
  typealias A = Never
}

// Default type witness
protocol SR_12707_P3 {
  associatedtype A
  associatedtype B: SR_12707_C<(A, Self)> = SR_12707_C<(A, Self)>
}
struct SR_12707_Conform_P3: SR_12707_P3 {
  typealias A = Never
}

// FIXME: Type witness resolution success is order-dependent.
protocol SR_12707_FIXME_P1 {
  associatedtype A
}
protocol SR_12707_FIXME_P2: SR_12707_FIXME_P1 {
  associatedtype B: SR_12707_C<(A, Self)> = SR_12707_C<(A, Self)> // expected-note {{default type 'associated_type.SR_12707_C<(associated_type.SR_12707_FIXME_Conform_P2.A, associated_type.SR_12707_FIXME_Conform_P2)>' (aka 'SR_12707_C<(Never, SR_12707_FIXME_Conform_P2)>') for associated type 'B' (from protocol 'SR_12707_FIXME_P2') does not inherit from 'associated_type.SR_12707_C<(associated_type.SR_12707_FIXME_Conform_P2.A, associated_type.SR_12707_FIXME_Conform_P2)>'}}
}
struct SR_12707_FIXME_Conform_P2: SR_12707_FIXME_P2 { // expected-error {{type 'SR_12707_FIXME_Conform_P2' does not conform to protocol 'SR_12707_FIXME_P2'}}
  typealias A = Never
}

// FIXME: Type witness resolution success is order-dependent.
protocol SR_12707_FIXME_P3 {
  associatedtype A: SR_12707_C<B> // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
  associatedtype B
}
struct SR_12707_FIXME_Conform_P3: SR_12707_FIXME_P3 { // expected-error {{type 'SR_12707_FIXME_Conform_P3' does not conform to protocol 'SR_12707_FIXME_P3'}}
  typealias A = SR_12707_C<B> // expected-note {{possibly intended match 'SR_12707_FIXME_Conform_P3.A' (aka 'SR_12707_C<Never>') does not inherit from 'SR_12707_C<SR_12707_FIXME_Conform_P3.B>'}}
  typealias B = Never
}
