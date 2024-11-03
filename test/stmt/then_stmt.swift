// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -enable-experimental-feature ThenStatements

// Required for regex
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ThenStatements

func then(_: Int = 0, x: Int = 0, fn: () -> Void = {}) {}

func testThenStmt(_ x: Int) {
  // These are statements
  then x // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then ()  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then (1)  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then (1, 2)  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then ""  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then []
  // expected-error@-1 {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  // expected-error@-2 {{empty collection literal requires an explicit type}}
  then [0]  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}

  then if .random() { 0 } else { 1 }
  // expected-error@-1 {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  // expected-error@-2 {{'if' may only be used as expression in return, throw, or as the source of an assignment}}

  then -1 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then ~1 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  then /abc/ // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}

  let x: Int = if .random() {
    then .zero
  } else {
    then 0
  }
  let y = if .random() { then x } else { then 1 }
  let _ = if .random() { (); then y } else { then 1 }

  // We don't allow labels on 'then' statements.
  let _ = switch Bool.random() {
  case true:
    a: then 0 // expected-error {{labels are only valid on loops, if, and switch statements}}
  case false:
    then 1
  }
}

func testThenFunctionCalls() {
  // These should all be treated as function calls.
  then()
  then(0)
  then(x: 0)
  then{}
  then {}
}

func testThenLabel() {
  then: for then in [0] { // expected-warning {{immutable value 'then' was never used; consider replacing with '_' or removing it}}
    break then
    continue then
  }
}

struct S {
  var then: Int
  func then(_ x: Int) {}

  subscript(x: Int) -> Void { () }

  mutating func testThenAsMember() -> Int {
    do {
      then  // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    } // expected-error {{expected expression after 'then'}}
    then // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    0 // expected-warning {{expression following 'then' is treated as an argument of the 'then'}}
    // expected-note@-1 {{indent the expression to silence this warning}}

    // Indented is okay.
    then // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
      0

    then;
    // expected-error@-1 {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    // expected-error@-2 {{expected expression after 'then'}}

    then . foo
    // expected-error@-1 {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    // expected-error@-2 {{reference to member 'foo' cannot be resolved without a contextual type}}

    // These are expressions.
    let _ = then
    let _ = self.then
    then + 1 // expected-warning {{result of operator '+' is unused}}
    then = 2

    (then) // expected-warning {{property is accessed but result is unused}}

    then is Int 
    // expected-warning@-1 {{'is' test is always true}}
    // expected-warning@-2 {{expression of type 'Bool' is unused}}
    then as Int 
    // expected-warning@-1 {{expression of type 'Int' is unused}}
    then as? Int
    // expected-warning@-1 {{conditional cast from 'Int' to 'Int' always succeeds}}
    // expected-warning@-2 {{expression of type 'Int?' is unused}}
    then as! Int
    // expected-warning@-1 {{forced cast of 'Int' to same type has no effect}}
    // expected-warning@-2 {{expression of type 'Int' is unused}}

    then ? 0 : 1 // expected-error {{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
    then! // expected-error {{cannot force unwrap value of non-optional type 'Int'}}
    then? // expected-error {{cannot use optional chaining on non-optional value of type 'Int'}}
    then.bitWidth // expected-warning {{expression of type 'Int' is unused}}
    then?.bitWidth // expected-error {{cannot use optional chaining on non-optional value of type 'Int'}}
    then!.bitWidth // expected-error {{cannot force unwrap value of non-optional type 'Int'}}

    // This is tricky because the lexer considers '/^' to be an infix operator.
    then /^ then/
    // expected-error@-1 {{cannot find operator '/^' in scope}}
    // expected-error@-2 {{'/' is not a postfix unary operator}}

    self.then(0)
    return then
  }
  func testThenSubscript() {
    let then = self
    then[0]
  }
}

func testOutOfPlace() -> Int {
  if .random() {
    guard .random() else {
      then 0 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    }
    if .random() {
      then 0 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    } else {
      then 1 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    }
    then 0 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
    then 0
  } else {
    then 1
  }
}

func testNested1() -> Int {
  if .random() {
    if .random() {
      then 1
    } else {
      then 2
    }
  } else {
    switch Bool.random() {
    case true:
      then 0
    case false:
      then 1
    }
  }
}

func testNested2() -> Int {
  if .random() {
    then if .random() {
      then 1
    } else {
      then 2
    }
  } else {
    then switch Bool.random() {
    case true:
      then 0
    case false:
      then 1
    }
  }
}

func testNested3() -> Int {
  if .random() {
    print("hello")
    then if .random() {
      then 1
    } else {
      then 2
    }
  } else {
    ()
    ()
    then switch Bool.random() {
    case true:
      then 0
    case false:
      then 1
    }
  }
}

func throwingFn() throws -> Int { 0 }

func testTryOnThen() throws -> Int {
  switch 0 {
  case 1:
    then try throwingFn() // okay
  default:
    try then() // expected-warning {{no calls to throwing functions occur within 'try' expression}}
    try then{} // expected-warning {{no calls to throwing functions occur within 'try' expression}}
    try then {} // expected-warning {{no calls to throwing functions occur within 'try' expression}}

    try then throwingFn()
    // expected-error@-1 {{'try' must be placed on the produced expression}} {{5-9=}} {{14-14=try }}
  }
}

func testReturnTryThen(_ then: Int) -> Int {
  return try then // expected-warning {{no calls to throwing functions occur within 'try' expression}}
}
