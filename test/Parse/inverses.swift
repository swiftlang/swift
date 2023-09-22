// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

protocol U {}

enum Maybe<Thing: ~Copyable> : ~Copyable {}

func more() {
  let _: any ~Copyable = 19

  let _: any ~Equatable = 19  // expected-error@:14 {{type 'Equatable' is not invertable}}

  let _: any (~Copyable & ~Equatable) // expected-error{{type 'Equatable' is not invertable}}

  let _: ~Any // expected-error {{type 'Any' is not invertable}}
  let _: ~AnyObject // expected-error {{type 'AnyObject' is not invertable}}
}

struct S4: ~(Copyable & Equatable) {} // expected-error {{type 'Copyable & Equatable' is not invertable}}

func blah<T>(_ t: T) where T: ~Copyable,
                           T: ~Hashable {}  // expected-error@:31 {{type 'Hashable' is not invertable}}

func foo<T: ~Copyable>(x: T) {}

struct Buurap<T: ~Copyable> {}

protocol Foo where Self: ~Copyable {
    func test<T>(_ t: T) where T: ~Self  // expected-error {{type 'Self' is not invertable}}
}

protocol Sando { func make() }

class C: ~Copyable,
         ~Sando // expected-error {{type 'Sando' is not invertable}}
         {}

public struct MoveOnlyS1<T> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS2<T: Equatable> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS3<T: ~Copyable> : ~Copyable { /*deinit {}*/ }

protocol Rope<Element>: Hashable, ~ Copyable {

  associatedtype Element: ~Copyable
}

extension S: ~Copyable {}

struct S: ~U, // expected-error {{type 'U' is not invertable}}
          ~Copyable {}

func greenBay<each T: ~Copyable>(_ r: repeat each T) {}

typealias Clone = Copyable
func dup<D: ~Clone>(_ d: D) {}

func superb(_ thing: some ~Copyable, thing2: some ~Clone) {}

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
typealias Z1 = ~Copyable.Type // expected-error {{type 'any Copyable.Type' is not invertable}}
typealias Z2 = ~A.B.C // expected-error {{type 'A.B.C' is not invertable}}
typealias Z3 = ~A? // expected-error {{type 'A?' is not invertable}}
typealias Z4 = ~Rope<Int> // expected-error {{type 'Rope<Int>' is not invertable}}
typealias Z5 = (~Int) -> Void // expected-error {{type 'Int' is not invertable}}
typealias Z6 = ~() -> () // expected-error {{single argument function types require parentheses}}
                         // expected-error@-1 {{type '()' is not invertable}}
typealias Z7 = ~(Copyable & Hashable) // expected-error {{type 'Copyable & Hashable' is not invertable}}
typealias Z8 = ~Copyable & Hashable
