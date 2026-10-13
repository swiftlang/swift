// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -Wwarning ImplicitForceUnwrap -verify-additional-prefix enabled-

// The ImplicitForceUnwrap warnings are off by default, so the first RUN line
// expects no diagnostics.

class C {
  var x: Int!
  var c: C!
  subscript(i: Int) -> Int! { x }
  func f() -> Int! { x }
  init() {}
  init!(fail: Bool) { if fail { return nil } }
  convenience init!(other: Bool) { self.init(fail: other) }
}

struct S {
  var v: Int!
}

func +(a: C, b: C) -> Int! { 1 }

func takeInt(_: Int) {}

func testExprs(c: C, s: S, o: Int!) {
  let _: Int = c.x // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{19-19=!}}
  let _: Int = s.v // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{19-19=!}}
  let _: Int = o // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{17-17=!}}
  let _: Int = c[0] // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{20-20=!}}
  let _: Int = c.f() // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{21-21=!}}
  let _: Int = c + c // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{16-16=(}}{{21-21=)!}}
  takeInt(o) // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{12-12=!}}
  _ = o + 1 // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{8-8=!}}

  // Each unwrap in a chain is diagnosed separately.
  let _: Int = c.c.x
  // expected-enabled-warning@-1{{implicit force unwrap of implicitly unwrapped optional of type 'C?'}}{{19-19=!}}
  // expected-enabled-warning@-2{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{21-21=!}}

  // Result of an init!.
  let _ = C(fail: false).x // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'C?'}}{{25-25=!}}

  // Inside closures.
  let _: Int = { c.x }() // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{21-21=!}}
  let _ = { () -> Int in
    let y: Int = c.x // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{21-21=!}}
    return y
  }

  // No implicit unwrap.
  let _: Int = c.x!
  let _: Int? = c.x
  let _ = c.x
  let _ = c.x?.description
  if let _ = c.x {}
}

func testKeyPaths() {
  let _: KeyPath<C, Int> = \C.x // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{32-32=!}}
  let _: KeyPath<C, Int> = \C.[0] // expected-enabled-warning{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{34-34=!}}
  let _: KeyPath<C, Int> = \C.c.x
  // expected-enabled-warning@-1{{implicit force unwrap of implicitly unwrapped optional of type 'C?'}}{{32-32=!}}
  // expected-enabled-warning@-2{{implicit force unwrap of implicitly unwrapped optional of type 'Int?'}}{{34-34=!}}

  // No implicit unwrap.
  let _: KeyPath<C, Int> = \C.x!
  let _: KeyPath<C, Int?> = \C.x
  let _: KeyPath<C, Int> = \C.c!.x!
}
