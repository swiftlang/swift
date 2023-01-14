// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros
protocol P { }
protocol Q { associatedtype Assoc }

@expression macro m1: Int = #externalMacro(module: "A", type: "M1")
// expected-warning@-1{{external macro implementation type 'A.M1' could not be found for macro 'm1'; the type must be public and provided via '-load-plugin-library'}}
@expression macro m2(_: Int) = #externalMacro(module: "A", type: "M2")
// expected-warning@-1{{external macro implementation type 'A.M2' could not be found for macro 'm2'; the type must be public and provided via '-load-plugin-library'}}
@expression macro m3(a b: Int) -> Int = #externalMacro(module: "A", type: "M3")
// expected-warning@-1{{external macro implementation type 'A.M3' could not be found for macro 'm3(a:)'; the type must be public and provided via '-load-plugin-library'}}
@expression macro m4<T: Q>: T = #externalMacro(module: "A", type: "M4") where T.Assoc: P
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm4'; the type must be public and provided via '-load-plugin-library'}}
@expression macro m5<T: P>(_: T) = #externalMacro(module: "A", type: "M4")
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm5'; the type must be public and provided via '-load-plugin-library'}}

@expression macro m6 = A // expected-error{{expected '(' for macro parameters or ':' for a value-like macro}}
// expected-error@-1{{macro must itself be defined by a macro expansion such as '#externalMacro(...)'}}

// expected-error @+2 {{expected '('}}
// expected-error @+1 {{macro 'm7' must declare its applicable contexts (e.g., '@expression')}}
@declaration macro m7(_: String) = #externalMacro(module: "A", type: "M4")
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm7'; the type must be public and provided via '-load-plugin-library'}}
// expected-error @+2 {{expected a declaration macro kind ('freestanding' or 'attached')}}
// expected-error @+1 {{macro 'm8' must declare its applicable contexts (e.g., '@expression')}}
@declaration(abc) macro m8(_: String) = #externalMacro(module: "A", type: "M4")
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm8'; the type must be public and provided via '-load-plugin-library'}}
@declaration(freestanding) macro m9(_: String) = #externalMacro(module: "A", type: "M4")
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm9'; the type must be public and provided via '-load-plugin-library'}}

@expression @declaration(freestanding) @attached(accessor)
macro m10(_: String) = #externalMacro(module: "A", type: "M4")
// expected-warning@-1{{external macro implementation type 'A.M4' could not be found for macro 'm10'; the type must be public and provided via '-load-plugin-library'}}


@attached(
  accessor,
  names: overloaded, arbitrary, named(hello), prefixed(_), suffixed(_favorite)
)
macro am1: Void
// expected-error@-1{{macro 'am1' requires a definition}}

@attached(
  accessor,
  overloaded, // expected-error{{@attached argument is missing label 'names'}}
  unknown, // expected-error{{unknown introduced name kind 'unknown'}}
  named, // expected-error{{introduced name kind 'named' requires a single argument '(name)'}}
  arbitrary(a) // expected-error{{introduced name kind 'arbitrary' must not have an argument}}
)
macro am2: Void
// expected-error@-1{{macro 'am2' requires a definition}}
