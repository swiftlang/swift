// RUN: %target-swift-frontend -module-name main -typecheck -verify -swift-version 4 %s

struct S: P {}
protocol P {}

let _: S.Type = type(of: S())
let _ = type(of: S())
let _: P.Type = type(of: S() as P)
let _ = type(of: S() as P)
let _: P.Protocol = type(of: S() as P) // expected-error{{}}

let _: S.Type = Swift.type(of: S())
let _ = Swift.type(of: S())
let _: P.Type = Swift.type(of: S() as P)
let _ = Swift.type(of: S() as P)
let _: P.Protocol = Swift.type(of: S() as P) // expected-error{{}}

let _: (S) -> S.Type = type(of:) // expected-error{{}}

func type(_: S) -> S {}
func type(kinda _: S) -> Any.Type {}

let _ = type(S())
let _: S = type(S())
let _ = type(kinda: S())
let _: Any.Type = type(kinda: S())

struct Q {}
struct R {}

func type(of: Q) -> R {}

let _: R = type(of: Q())
let _: Q.Type = type(of: Q())
let _: Q.Type = Swift.type(of: Q())
let _: R = Swift.type(of: Q()) // expected-error{{}}
let _: Q.Type = main.type(of: Q()) // expected-error{{}}
let _: R = main.type(of: Q()) 

// Let's make sure that binding of the left-hand side
// of the dynamic-type-of constraint is not attempted.

class C {
   typealias T = Int
}

class D : C {
   typealias T = Float
}

func foo(_: Any...) {}

func bar() -> Int { return 42 }    // expected-note {{found this candidate}}
func bar() -> Float { return 0.0 } // expected-note {{found this candidate}}

foo(type(of: D.T.self)) // Ok
let _: Any = type(of: D.T.self) // Ok
foo(type(of: bar())) // expected-error {{ambiguous use of 'bar()'}}
