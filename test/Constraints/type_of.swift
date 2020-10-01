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

// We need at least 4 classes here because type(of:)
// has 3 declarations in this file, and we need to
// try and make it so type(of:) picked as first overload.

class D : C {
   typealias T = Float
}

class E : D {
   typealias T = Double
}

class F : E {
   typealias T = UInt
}

class G : F {
   typealias T = Float
}

func foo(_: Any...) {}

// It's imperative for bar() to have more overloads
// the that of type(of:) to make sure that latter is
// picked first.

func bar() -> Int {}    // expected-note {{found this candidate}}
func bar() -> Float {}  // expected-note {{found this candidate}}
func bar() -> String {} // expected-note {{found this candidate}}
func bar() -> UInt {}   // expected-note {{found this candidate}}

foo(type(of: G.T.self)) // Ok
let _: Any = type(of: G.T.self) // Ok
foo(type(of: bar())) // expected-error {{ambiguous use of 'bar()'}}

struct SR10696 {
  func bar(_ s: SR10696.Type) {
    type(of: s)() // expected-error {{type 'SR10696.Type' has no member 'init'}}
  }
}
