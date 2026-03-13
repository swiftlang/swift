// RUN: %target-typecheck-verify-swift -swift-version 4

class C { }

protocol P {
  associatedtype AssocP : C // expected-note{{protocol requires nested type 'AssocP'}}
  associatedtype AssocA : AnyObject // expected-note{{protocol requires nested type 'AssocA'}}
}

struct X : P { // expected-error{{type 'X' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  typealias AssocP = Int // expected-note{{possibly intended match 'X.AssocP' (aka 'Int') does not inherit from 'C'}}
  typealias AssocA = Int // expected-note{{possibly intended match 'X.AssocA' (aka 'Int') does not conform to 'AnyObject'}}
}

// https://github.com/apple/swift/issues/47742
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
// (and @Sendable equivalents of some cases)
protocol P1 {
  associatedtype A

  func f(_: A) // expected-note {{expected sendability to match requirement here}}
}

struct X1a : P1 {
  func f(_: @escaping (Int) -> Int) { }
}

struct SendableX1a : P1 {
  func f(_: @Sendable (Int) -> Int) { }
}

struct X1b : P1 {
  typealias A = (Int) -> Int

  func f(_: @escaping (Int) -> Int) { }
}

// FIXME: We would like this case to work. It doesn't because, by the time we
//        actually look through the DependentMemberType and see that `A` is a
//        @Sendable function type, we are simplifying a Bind constraint and
//        don't have the flexibility to adjust the witness's type. Perhaps
//        instead of adjusting types before adding them to the constraint
//        graph, we should introduce a new constraint kind that allows only a
//        witness's adjustments.
struct SendableX1b : P1 {
  typealias A = @Sendable (Int) -> Int

  func f(_: (Int) -> Int) { } // expected-warning {{sendability of function types in instance method 'f' does not match requirement in protocol 'P1'}}
}

struct X1c : P1 {
  typealias A = (Int) -> Int

  func f(_: (Int) -> Int) { }
}

struct X1d : P1 {
  func f(_: (Int) -> Int) { }
}

protocol P2 {
  func f(_: (Int) -> Int) // expected-note{{expected sendability to match requirement here}} expected-note 2{{protocol requires function 'f' with type '((Int) -> Int) -> ()'}}
  func g(_: @escaping (Int) -> Int) // expected-note 2 {{expected sendability to match requirement here}}
  func h(_: @Sendable (Int) -> Int) // expected-note 2 {{protocol requires function 'h' with type '(@Sendable (Int) -> Int) -> ()'}}
  func i(_: @escaping @Sendable (Int) -> Int)
}

struct X2a : P2 {
  func f(_: (Int) -> Int) { }
  func g(_: (Int) -> Int) { }
  func h(_: (Int) -> Int) { }
  func i(_: (Int) -> Int) { }
}

struct X2b : P2 { // expected-error{{type 'X2b' does not conform to protocol 'P2'}} expected-note {{add stubs for conformance}}
  func f(_: @escaping (Int) -> Int) { } // expected-note{{candidate has non-matching type '(@escaping (Int) -> Int) -> ()'}}
  func g(_: @escaping (Int) -> Int) { }
  func h(_: @escaping (Int) -> Int) { } // expected-note{{candidate has non-matching type '(@escaping (Int) -> Int) -> ()'}}
  func i(_: @escaping (Int) -> Int) { }
}

struct X2c : P2 {
  func f(_: @Sendable (Int) -> Int) { } // expected-warning{{sendability of function types in instance method 'f' does not match requirement in protocol 'P2'}}
  func g(_: @Sendable (Int) -> Int) { } // expected-warning{{sendability of function types in instance method 'g' does not match requirement in protocol 'P2'}}
  func h(_: @Sendable (Int) -> Int) { }
  func i(_: @Sendable (Int) -> Int) { }
}

struct X2d : P2 { // expected-error{{type 'X2d' does not conform to protocol 'P2'}} expected-note {{add stubs for conformance}}
  func f(_: @escaping @Sendable (Int) -> Int) { } // expected-note{{candidate has non-matching type '(@escaping @Sendable (Int) -> Int) -> ()'}}
  func g(_: @escaping @Sendable (Int) -> Int) { } // expected-warning{{sendability of function types in instance method 'g' does not match requirement in protocol 'P2'}}
  func h(_: @escaping @Sendable (Int) -> Int) { } // expected-note{{candidate has non-matching type '(@escaping @Sendable (Int) -> Int) -> ()'}}
  func i(_: @escaping @Sendable (Int) -> Int) { }
}


// https://github.com/apple/swift/issues/55151

class GenClass<T> {}

// Regular type witnesses
protocol P3a {
  associatedtype A
  associatedtype B: GenClass<(A, Self)> // expected-note {{'B' declared here}}
}
struct S3a: P3a {
  typealias A = Never
  typealias B = GenClass<(A, S3a)>
}

// Type witness in protocol extension
protocol P3b: P3a {}
extension P3b {
  typealias B = GenClass<(A, Self)> // expected-warning {{typealias overriding associated type 'B' from protocol 'P3a' is better expressed as same-type constraint on the protocol}}
}
struct S3b: P3b {
  typealias A = Never
}

protocol P4 {
  associatedtype A: GenClass<B>
  associatedtype B
}
struct S4: P4 {
  typealias A = GenClass<B>
  typealias B = Never
}

// Associated type inference via value witnesses should consider tentative
// witnesses when checking a candidate.
protocol P5 {
  associatedtype X = Never

  associatedtype A: GenClass<X>
  func foo(arg: A)
}
struct S5: P5 {
  func foo(arg: GenClass<Never>) {}
}

// Abstract type witnesses.
protocol P6a {
  associatedtype X = Never

  associatedtype A: GenClass<X>
  associatedtype B: GenClass<X>
}
protocol P6b: P6a where B == GenClass<X> {
  associatedtype C: GenClass<Self> = GenClass<Self>
}
struct S6<A: GenClass<Never>>: P6b {}
