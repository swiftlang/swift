// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

protocol Operator {
  func +(lhs: Self, rhs: Self) -> Self // expected-error {{operator '+' declared in protocol must be 'static'}}
}

func foo(x: Int..., _: String) {} // expected-error {{a parameter following a variadic parameter requires a label}}

protocol P1 {}
protocol P2 {}

let x: protocol<> // expected-error {{'protocol<>' syntax has been removed; use 'Any' instead}}
let y: protocol<P1> // expected-error {{'protocol<...>' composition syntax has been removed and is not needed here}}}
let z: protocol<P1, P2> // expected-error {{'protocol<...>' composition syntax has been removed; join the protocols using '&'}}

func bar(f: @noescape () -> ()) {} // expected-error {{@noescape is the default and has been removed}}

func baz(f: @autoclosure(escaping) () -> ()) {} // expected-error {{@autoclosure(escaping) has been removed; use @autoclosure @escaping instead}}

prefix operator +++ {} // expected-error {{operator should no longer be declared with body}}
postfix operator +++ {} // expected-error {{operator should no longer be declared with body}}
infix operator +++ {} // expected-error {{operator should no longer be declared with body}}
infix operator +++* { // expected-error {{operator should no longer be declared with body; use a precedence group instead}}
  associativity right
}
