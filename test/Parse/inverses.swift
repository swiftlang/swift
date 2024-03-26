// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

protocol U {}

enum Maybe<Thing: ~Copyable> : ~Copyable {}

func more() {
  let _: any ~Copyable = 19

  let _: any ~Equatable = 19  // expected-error@:14 {{type 'Equatable' cannot be suppressed}}

  let _: any (~Copyable & ~Equatable) // expected-error{{type 'Equatable' cannot be suppressed}}

  let _: ~Any // expected-error {{type 'Any' cannot be suppressed}}
  let _: ~AnyObject // expected-error {{type 'AnyObject' cannot be suppressed}}
}

struct S4: ~(Copyable & Equatable) {} // expected-error {{type 'Equatable' cannot be suppressed}}

func blah<T>(_ t: borrowing T) where T: ~Copyable,
                                     T: ~Hashable {}  // expected-error@:41 {{type 'Hashable' cannot be suppressed}}

func foo<T: ~Copyable>(x: borrowing T) {}

struct Buurap<T: ~Copyable> where T: ~Copyable {}

struct ExtraNoncopyStruct: ~Copyable, ~Copyable {}
struct ExtraNoncopyEnum: ~Copyable, ~Copyable {}

protocol ExtraNoncopyProto: ~Copyable, ~Copyable {}

protocol Foo: ~Copyable
         where Self: ~Copyable {

    associatedtype Touch : ~Copyable,
                           ~Copyable

    func test<T>(_ t: T) where T: ~Self  // expected-error {{type 'Self' cannot be suppressed}}
}

protocol Sando { func make() }

class C: ~Copyable,  // expected-error {{classes cannot be '~Copyable'}}
         ~Sando // expected-error {{type 'Sando' cannot be suppressed}}
         {}

public struct MoveOnlyS1<T> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS2<T: Equatable> : ~Copyable { /*deinit {}*/ }
public struct MoveOnlyS3<T: ~Copyable> : ~Copyable { /*deinit {}*/ }

protocol Rope<Element>: Hashable, ~Copyable {  // expected-error {{'Self' required to be 'Copyable' but is marked with '~Copyable'}}
  associatedtype Element: ~Copyable
}

extension S: ~Copyable {} // expected-error {{cannot suppress '~Copyable' in extension}}

struct S: ~U, // expected-error {{type 'U' cannot be suppressed}}
          ~Copyable {}

func greenBay<each T: ~Copyable>(_ r: repeat each T) {} // expected-error{{cannot suppress '~Copyable' on type 'each T'}}

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

func ownership1(_ t: borrowing any ~Equatable) {} // expected-error {{type 'Equatable' cannot be suppressed}}

func ownership2(_ t: ~ borrowing Int) {} // expected-error {{cannot find type 'borrowing' in scope}}
                                         // expected-error@-1 {{unnamed parameters must be written with the empty name '_'}}
                                         // expected-error@-2 {{expected ',' separator}}

func ownership3(_ t: consuming some ~Clone) {}

func what(one: ~Copyable..., // expected-error {{type 'any Copyable' cannot be suppressed}}
          two: ~(Copyable...) // expected-error {{variadic parameter cannot appear outside of a function parameter list}}
                              // expected-error@-1 {{type 'any Copyable' cannot be suppressed}}
          ) {}

struct A { struct B { struct C {} } }

typealias Z1 = (~Copyable).Type // FIXME: should be an error
typealias Z1 = ~Copyable.Type // expected-error {{type 'any Copyable.Type' cannot be suppressed}}
typealias Z2 = ~A.B.C // expected-error {{type 'A.B.C' cannot be suppressed}}
typealias Z3 = ~A? // expected-error {{type 'A?' cannot be suppressed}}
typealias Z4 = ~Rope<Int> // expected-error {{type 'Rope<Int>' cannot be suppressed}}
typealias Z5 = (~Int) -> Void // expected-error {{type 'Int' cannot be suppressed}}
typealias Z6 = ~() -> () // expected-error {{single argument function types require parentheses}}
                         // expected-error@-1 {{type '()' cannot be suppressed}}
typealias Z7 = ~(Copyable & Hashable) // expected-error {{type 'Hashable' cannot be suppressed}}
typealias Z8 = ~Copyable & Hashable // expected-error {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}

struct NotAProtocol {}

struct Bad: ~NotAProtocol {} // expected-error {{type 'NotAProtocol' cannot be suppressed}}