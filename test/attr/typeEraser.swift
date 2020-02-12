// RUN: %target-swift-frontend -typecheck %s -verify

class AnyP: P1 {}

@_typeEraser(AnyP) // okay
protocol P1 {}

@_typeEraser // expected-error {{expected '(' in '_typeEraser' attribute}}
protocol P2 {}

@_typeEraser() // expected-error {{expected a type name in @_typeEraser()}}
protocol P3 {}

@_typeEraser(AnyP // expected-note {{to match this opening '('}}
protocol P4 {} // expected-error {{expected ')' after type name for @_typeEraser}}

@_typeEraser(AnyP) // expected-error {{@_typeEraser may only be used on 'protocol' declarations}}
func notAProtocol() {}

