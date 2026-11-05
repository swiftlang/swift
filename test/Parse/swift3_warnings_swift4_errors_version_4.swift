// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

protocol Operator {
  func +(lhs: Self, rhs: Self) -> Self // expected-cparser-error {{operator '+' declared in protocol must be 'static'}} expected-astgen-error {{operator '+' declared in type 'Operator' must be 'static'}}
}

func foo(x: Int..., _: String) {} // expected-error {{a parameter following a variadic parameter requires a label}}

protocol P1 {}
protocol P2 {}

let x: protocol<> // expected-error {{'protocol<>' syntax has been removed; use 'Any' instead}}
let y: protocol<P1> // expected-error {{'protocol<...>' composition syntax has been removed and is not needed here}}}
let z: protocol<P1, P2> // expected-error {{'protocol<...>' composition syntax has been removed; join the type constraints using '&'}}

func bar(f: @noescape () -> ()) {} // expected-error {{unknown attribute 'noescape'}}

func baz(f: @autoclosure(escaping) () -> ()) {} // expected-astgen-note {{to match this opening '('}}
// expected-error @-1 {{cannot find type 'escaping' in scope}}
// expected-cparser-error @-2 {{unnamed parameters must be written with the empty name '_'}} expected-astgen-error@-2 {{unexpected code '() -> ())' in function}}
// expected-cparser-error @-3 {{expected ',' separator}} expected-astgen-error@-3 {{expected ')' to end parameter clause}}

prefix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} expected-astgen-error {{operator should not be declared with body}}
postfix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} expected-astgen-error {{operator should not be declared with body}}
infix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} expected-astgen-error {{operator should not be declared with body}}
infix operator +++* { // expected-cparser-error {{operator should no longer be declared with body; use a precedence group instead}} expected-astgen-error {{operator should not be declared with body}}
  associativity right
}
