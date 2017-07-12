// RUN: %target-swift-frontend -typecheck -verify -swift-version 3 %s

protocol Operator {
  func +(lhs: Self, rhs: Self) -> Self // expected-warning {{operator '+' declared in protocol must be 'static'}}
}

func foo(x: Int..., _: String) {} // expected-warning {{a parameter following a variadic parameter requires a label}}

protocol P1 {}
protocol P2 {}

let x: protocol<> // expected-warning {{'protocol<>' syntax is deprecated; use 'Any' instead}}
let y: protocol<P1> // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}}}
let z: protocol<P1, P2> // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}}

func bar(f: @noescape () -> ()) {} // expected-warning {{@noescape is the default and is deprecated}}

func baz(f: @autoclosure(escaping) () -> ()) {} // expected-warning {{@autoclosure(escaping) is deprecated; use @autoclosure @escaping instead}}

prefix operator +++ {} // expected-warning {{operator should no longer be declared with body}}
postfix operator +++ {} // expected-warning {{operator should no longer be declared with body}}
infix operator +++ {} // expected-warning {{operator should no longer be declared with body}}
infix operator +++* { // expected-warning {{operator should no longer be declared with body; use a precedence group instead}}
  associativity right
}

