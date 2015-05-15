// RUN: %target-parse-verify-swift -parse-as-library

let _ = 1 // expected-error{{global variable declaration does not bind any variables}}

func foo() {
  let _ = 1 // OK
}

struct Foo {
  let _ = 1 // expected-error{{property declaration does not bind any variables}}
  var (_, _) = (1, 2) // expected-error{{property declaration does not bind any variables}}

  func foo() {
    let _ = 1 // OK
  }
}


// <rdar://problem/19786845> Warn on "let" and "var" when no data is bound in a pattern
enum SimpleEnum { case Bar }


func testVarLetPattern(a : SimpleEnum) {
  switch a {
  case let .Bar: break      // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}}
  }
  switch a {
  case let x: _ = x; break         // Ok.
  }
  switch a {
  case let _: break         // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}}
  }
  switch (a, 42) {
  case let (_, x): _ = x; break    // ok
  }
}
