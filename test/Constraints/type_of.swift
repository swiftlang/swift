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
