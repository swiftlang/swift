// RUN: %target-typecheck-verify-swift -parse-as-library

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
  case let .Bar: break      // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}} {{8-12=}}
  }
  switch a {
  case let x: _ = x; break         // Ok.
  }
  switch a {
  case let _: break         // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}} {{8-12=}}
  }
  switch (a, 42) {
  case let (_, x): _ = x; break    // ok
  }
  // expected-warning @+1 {{'if' condition is always true}}
  if case let _ = "str" {}  // expected-warning {{'let' pattern has no effect; sub-pattern didn't bind any variables}} {{11-15=}}
}

// https://github.com/apple/swift/issues/53293
class C_53293 {
  static var _: Int { 0 } //expected-error {{getter/setter can only be defined for a single variable}}
}
