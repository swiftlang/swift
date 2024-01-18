// RUN: %target-typecheck-verify-swift

// Currently disabled by default.

func throwsError() throws -> Int { 0 }

func test1() -> Int {
  return do { 5 }
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{non-void function should return a value}}
  // expected-warning@-3 {{integer literal is unused}}
}

func test2() -> Int {
  return do { try throwsError() } catch { 0 }
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{non-void function should return a value}}
  // expected-warning@-3 {{integer literal is unused}}
  // expected-warning@-4 {{result of call to 'throwsError()' is unused}}
}

func test3() -> Int {
  return
  do { 5 }
  // expected-error@-2 {{non-void function should return a value}}
  // expected-warning@-2 {{integer literal is unused}}
}

func test4() -> Int {
  return
  do { try throwsError() } catch { 0 }
  // expected-error@-2 {{non-void function should return a value}}
  // expected-warning@-2 {{integer literal is unused}}
  // expected-warning@-3 {{result of call to 'throwsError()' is unused}}
}

func test5() -> Int {
  do { 5 } // expected-warning {{integer literal is unused}}
}

func test6() -> Int {
  do { try throwsError() } catch { 0 }
  // expected-warning@-1 {{integer literal is unused}}
  // expected-warning@-2 {{result of call to 'throwsError()' is unused}}
}

func test7() -> Int {
  do { 5 } as Int
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-warning@-2 {{integer literal is unused}}
  // expected-error@-3 {{expected expression}}
}

func test8() -> Int {
  do { try throwsError() } catch { 0 } as Int
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-warning@-2 {{integer literal is unused}}
  // expected-error@-3 {{expected expression}}
  // expected-warning@-4 {{result of call to 'throwsError()' is unused}}
}

func test9() -> Int {
  let x = do { 5 }
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected initial value after '='}}
  // expected-warning@-3 {{integer literal is unused}}

  return x
}

func test10() -> Int {
  let x = do { try throwsError() } catch { 0 }
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected initial value after '='}}
  // expected-warning@-3 {{integer literal is unused}}
  // expected-warning@-4 {{result of call to 'throwsError()' is unused}}

  return x
}

func test11() -> Int {
  let fn = { do { 5 } }
  // expected-warning@-1 {{integer literal is unused}}

  return fn() // expected-error {{cannot convert return expression of type '()' to return type 'Int'}}
}

func test12() -> Int {
  let fn = { do { try throwsError() } catch { 0 } }
  // expected-warning@-1 {{integer literal is unused}}
  // expected-warning@-2 {{result of call to 'throwsError()' is unused}}

  return fn() // expected-error {{cannot convert return expression of type '()' to return type 'Int'}}
}

func test13() -> Int {
  let x = if .random() {
    do { 0 } // expected-warning {{integer literal is unused}}
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    1
  }
  return x
}

func test14() -> Int {
  let x = if .random() {
    1
  } else {
    do { 2 } catch { 3 } // expected-warning 2{{integer literal is unused}}
    // expected-warning@-1 {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  } // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  return x
}

func test15() -> Int {
  if .random() {
    do { 0 } // expected-warning {{integer literal is unused}}
  } else {
    1 // expected-warning {{integer literal is unused}}
  }
}

func test16() -> Int {
  if .random() {
    1 // expected-warning {{integer literal is unused}}
  } else {
    do { 2 } catch { 3 } // expected-warning 2{{integer literal is unused}}
    // expected-warning@-1 {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  }
}
