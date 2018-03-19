// RUN: %target-typecheck-verify-swift -swift-version 3
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
  // expected-note@-1 {{protocol requires function 'f' with type '(X1c.A) -> ()' (aka '((Int) -> Int) -> ()'); do you want to add a stub?}}
  // expected-note@-2 {{protocol requires function 'f' with type '((Int) -> Int) -> ()'; do you want to add a stub?}}
}

struct X1a : P1 {
  func f(_: @escaping (Int) -> Int) { }
}

struct X1b : P1 {
  typealias A = (Int) -> Int

  func f(_: @escaping (Int) -> Int) { }
}

struct X1c : P1 { // expected-error{{type 'X1c' does not conform to protocol 'P1'}}
  typealias A = (Int) -> Int

  func f(_: (Int) -> Int) { } // expected-note{{candidate has non-matching type '((Int) -> Int) -> ()'}}
}

struct X1d : P1 { // expected-error{{type 'X1d' does not conform to protocol 'P1'}}
  func f(_: (Int) -> Int) { } // expected-note{{candidate has non-matching type '((Int) -> Int) -> ()' [with A = (Int) -> Int]}}
}
