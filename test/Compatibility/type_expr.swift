// RUN: %target-typecheck-verify-swift -swift-version 3

func takesOneArg<T>(_: T.Type) {}
func takesTwoArgs<T>(_: T.Type, _: Int) {}

func testMissingSelf() {
  // None of these were not caught in Swift 3.
  // See test/Compatibility/type_expr.swift.

  takesOneArg(Int)
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  takesOneArg(Swift.Int)
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  takesTwoArgs(Int, 0)
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  takesTwoArgs(Swift.Int, 0)
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  Swift.Int // expected-warning {{expression of type 'Int.Type' is unused}}
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  _ = Swift.Int
  // expected-warning@-1 {{missing '.self' for reference to metatype of type 'Int'}}

  Int // expected-warning {{expression of type 'Int.Type' is unused}}
  // expected-error@-1 {{expected member name or constructor call after type name}}
  // expected-note@-2 {{add arguments after the type to construct a value of the type}}
  // expected-note@-3 {{use '.self' to reference the type object}}

  _ = Int
  // expected-error@-1 {{expected member name or constructor call after type name}}
  // expected-note@-2 {{add arguments after the type to construct a value of the type}}
  // expected-note@-3 {{use '.self' to reference the type object}}
}
