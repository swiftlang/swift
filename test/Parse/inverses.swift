// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics -warn-redundant-requirements

// REQUIRES: asserts

protocol U {}

enum Maybe<Thing: ~Copyable> : ~Copyable {}

func more() {
  let _: any ~Copyable = 19

  let _: any ~Equatable = 19  // expected-error@:14 {{type 'Equatable' is not invertible}}

  let _: any (~Copyable & ~Equatable) // expected-error{{type 'Equatable' is not invertible}}

  let _: ~Any // expected-error {{type 'Any' is not invertible}}
  let _: ~AnyObject // expected-error {{type 'AnyObject' is not invertible}}
}

struct S4: ~(Copyable & Equatable) {} // expected-error {{type 'Copyable & Equatable' is not invertible}}

func blah<T>(_ t: borrowing T) where T: ~Copyable,
                                     T: ~Hashable {}  // expected-error@:41 {{type 'Hashable' is not invertible}}

func foo<T: ~Copyable>(x: borrowing T) {}

struct Buurap<T: ~Copyable> where T: ~Copyable {}
// expected-warning@-1 {{redundant conformance constraint 'T' : '~Copyable'}}

struct ExtraNoncopyStruct: ~Copyable, ~Copyable {}
struct ExtraNoncopyEnum: ~Copyable, ~Copyable {}

protocol ExtraNoncopyProto: ~Copyable, ~Copyable {}
// expected-warning@-1 {{redundant conformance constraint 'Self' : '~Copyable'}}

protocol Foo: ~Copyable
         where Self: ~Copyable {
         // expected-warning@-1 {{redundant conformance constraint 'Self' : '~Copyable'}}

    associatedtype Touch : ~Copyable,
                           ~Copyable
    // expected-warning@-1 {{redundant conformance constraint 'Self.Touch' : '~Copyable'}}

    func test<T>(_ t: T) where T: ~Self  // expected-error {{type 'Self' is not invertible}}
}

protocol Sando { func make() }

class C: ~Copyable,  // expected-error {{classes cannot be noncopyable}}
         ~Sando // expected-error {{type 'Sando' is not invertible}}
         {}

public struct MoveOnlyS1<T> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS2<T: Equatable> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS3<T: ~Copyable> : ~Copyable { /*deinit {}*/ }

protocol Rope<Element>: Hashable, ~ Copyable {

  associatedtype Element: ~Copyable
}

extension S: ~Copyable {} // expected-error {{cannot apply inverse '~Copyable' to extension}}

struct S: ~U, // expected-error {{type 'U' is not invertible}}
          ~Copyable {}

func greenBay<each T: ~Copyable>(_ r: repeat each T) {} // expected-error{{cannot apply inverse '~Copyable' to type 'each T' in conformance requirement}}

typealias Clone = Copyable
func dup<D: ~Clone>(_ d: D) {}
// expected-error@-1 {{parameter of noncopyable type 'D' must specify ownership}}
// expected-note@-2 {{add 'borrowing'}}
// expected-note@-3 {{add 'inout'}}
// expected-note@-4 {{add 'consuming'}}

// expected-error@+2 {{parameter of noncopyable type 'some ~Copyable' must specify ownership}}
// expected-error@+1 {{parameter of noncopyable type 'some ~Clone' must specify ownership}}
func superb(_ thing: some ~Copyable, thing2: some ~Clone) {}
// expected-note@-1 2{{add 'borrowing'}}
// expected-note@-2 2{{add 'inout'}}
// expected-note@-3 2{{add 'consuming'}}

func ownership1(_ t: borrowing any ~Equatable) {} // expected-error {{type 'Equatable' is not invertible}}

func ownership2(_ t: ~ borrowing Int) {} // expected-error {{cannot find type 'borrowing' in scope}}
                                         // expected-error@-1 {{unnamed parameters must be written with the empty name '_'}}
                                         // expected-error@-2 {{expected ',' separator}}

func ownership3(_ t: consuming some ~Clone) {}

func what(one: ~Copyable..., // expected-error {{type 'any Copyable' is not invertible}}
          two: ~(Copyable...) // expected-error {{variadic parameter cannot appear outside of a function parameter list}}
                              // expected-error@-1 {{type 'any Copyable' is not invertible}}
          ) {}

struct A { struct B { struct C {} } }

typealias Z1 = (~Copyable).Type // FIXME: should be an error
typealias Z1 = ~Copyable.Type // expected-error {{type 'any Copyable.Type' is not invertible}}
typealias Z2 = ~A.B.C // expected-error {{type 'A.B.C' is not invertible}}
typealias Z3 = ~A? // expected-error {{type 'A?' is not invertible}}
typealias Z4 = ~Rope<Int> // expected-error {{type 'Rope<Int>' is not invertible}}
typealias Z5 = (~Int) -> Void // expected-error {{type 'Int' is not invertible}}
typealias Z6 = ~() -> () // expected-error {{single argument function types require parentheses}}
                         // expected-error@-1 {{type '()' is not invertible}}
typealias Z7 = ~(Copyable & Hashable) // expected-error {{type 'Copyable & Hashable' is not invertible}}
typealias Z8 = ~Copyable & Hashable
