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
