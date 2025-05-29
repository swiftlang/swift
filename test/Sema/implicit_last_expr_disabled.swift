// RUN: %target-typecheck-verify-swift

// Currently disabled by default

let a = if .random() {
  print("hello")
  6 // expected-warning {{integer literal is unused}}
} else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  7
}

let b = if .random() {
  // expected-warning@-1 {{constant 'b' inferred to have type 'Void', which may be unexpected}}
  // expected-note@-2 {{add an explicit type annotation to silence this warning}}
  print("hello")
  if .random() { 5 } else { 6 } // expected-warning 2{{integer literal is unused}}
} else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  let x = 7
  x // expected-warning {{expression of type 'Int' is unused}}
} // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}

let c = switch Bool.random() {
  // expected-warning@-1 {{constant 'c' inferred to have type 'Void', which may be unexpected}}
  // expected-note@-2 {{add an explicit type annotation to silence this warning}}
case true:
  print("hello")
  6 // expected-warning {{integer literal is unused}}
case false: // expected-error@-1 {{non-expression branch of 'switch' expression may only end with a 'throw'}}
  ()
  7 // expected-warning {{integer literal is unused}}
} // expected-error@-1 {{non-expression branch of 'switch' expression may only end with a 'throw'}}

let d = switch Bool.random() {
case true:
  print("hello")
  if .random() { 5 } else { 6 } // expected-warning 2{{integer literal is unused}}
case false: // expected-error@-1 {{non-expression branch of 'switch' expression may only end with a 'throw'}}
  7
}

let e = if .random() {
  // expected-warning@-1 {{constant 'e' inferred to have type 'Void', which may be unexpected}}
  // expected-note@-2 {{add an explicit type annotation to silence this warning}}
  if .random() {
    ()
    1 // expected-warning {{integer literal is unused}}
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    ()
    2 // expected-warning {{integer literal is unused}}
  } // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
} else {
  switch Bool.random() {
  case true:
    ()
    0 // expected-warning {{integer literal is unused}}
  case false: // expected-error@-1 {{non-expression branch of 'switch' expression may only end with a 'throw'}}
    ()
    1 // expected-warning {{integer literal is unused}}
  } // expected-error@-1 {{non-expression branch of 'switch' expression may only end with a 'throw'}}
}

func testFn1() -> Int {
  print("hello")
  0 // expected-warning {{integer literal is unused}}
}

func takesFn(_ fn: () -> Int) {}

func testClosure1() {
  takesFn {
    ()
    0 // expected-warning {{integer literal is unused}}
  }
}

func testClosure2() {
  let fn = {
    ()
    if .random() { 0 } else { 1 } // expected-warning 2{{integer literal is unused}}
  }
  takesFn(fn) // expected-error {{cannot convert value of type '() -> ()' to expected argument type '() -> Int'}}
}
