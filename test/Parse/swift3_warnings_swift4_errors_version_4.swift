// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

protocol Operator {
  func +(lhs: Self, rhs: Self) -> Self // expected-error {{operator '+' declared in protocol must be 'static'}}
}

func foo(x: Int..., _: String) {} // expected-error {{a parameter following a variadic parameter requires a label}}

protocol P1 {}
protocol P2 {}

let x: protocol<> // expected-error {{'protocol<>' syntax has been removed; use 'Any' instead}}
let y: protocol<P1> // expected-error {{'protocol<...>' composition syntax has been removed and is not needed here}}}
let z: protocol<P1, P2> // expected-error {{'protocol<...>' composition syntax has been removed; join the type constraints using '&'}}

func bar(f: @noescape () -> ()) {} // expected-error {{unknown attribute 'noescape'}}

func baz(f: @autoclosure(escaping) () -> ()) {}
// expected-error @-1 {{cannot find type 'escaping' in scope}}
// expected-error @-2 {{unnamed parameters must be written with the empty name '_'}}
// expected-error @-3 {{expected ',' separator}}

prefix operator +++ {} // expected-error {{operator should no longer be declared with body}}
postfix operator +++ {} // expected-error {{operator should no longer be declared with body}}
infix operator +++ {} // expected-error {{operator should no longer be declared with body}}
infix operator +++* { // expected-error {{operator should no longer be declared with body; use a precedence group instead}}
  associativity right
}
