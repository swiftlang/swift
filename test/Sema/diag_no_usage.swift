// RUN: %target-typecheck-verify-swift -verify-additional-prefix noerror-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix werror- -Werror NoUsage

// MARK: - Unused literals

func testUnusedLiterals() {
  42
  // expected-noerror-warning@-1 {{integer literal is unused}}
  // expected-werror-error@-2 {{integer literal is unused}}
  "hello"
  // expected-noerror-warning@-1 {{string literal is unused}}
  // expected-werror-error@-2 {{string literal is unused}}
}

// MARK: - Unused function call result

func returnsInt() -> Int { return 42 }

func testUnusedResult() {
  returnsInt()
  // expected-noerror-warning@-1 {{result of call to 'returnsInt()' is unused}}
  // expected-werror-error@-2 {{result of call to 'returnsInt()' is unused}}
}

// MARK: - Unused initializer result

class MyClass {}

func testUnusedInitResult() {
  MyClass()
  // expected-noerror-warning@-1 {{result of 'MyClass' initializer is unused}}
  // expected-werror-error@-2 {{result of 'MyClass' initializer is unused}}
}

// MARK: - Unused variable binding

func testUnusedVariable() {
  let x = 5
  // expected-noerror-warning@-1 {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
  // expected-werror-error@-2 {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
}

// MARK: - Unused pattern binding

func testUnusedPatternBinding(_ pair: (Int, String)) {
  switch pair {
  case (let n, _): break // expected-noerror-warning {{immutable value 'n' was never used; consider replacing with '_' or removing it}}
                         // expected-werror-error@-1 {{immutable value 'n' was never used; consider replacing with '_' or removing it}}
  }
}

// MARK: - Unused closure capture

func testUnusedCapture() {
  let value = 42
  let _ = { [value] in 0 }
  // expected-noerror-warning@-1 {{capture 'value' was never used}}
  // expected-werror-error@-2 {{capture 'value' was never used}}
}

// MARK: - Unused optional try result

enum SomeError: Error { case problem }
func throwingFunc() throws -> Int { throw SomeError.problem }

func testUnusedOptionalTry() {
  try? throwingFunc()
  // expected-noerror-warning@-1 {{result of 'try?' is unused}}
  // expected-werror-error@-2 {{result of 'try?' is unused}}
}

// MARK: - Unused value in condition binding

func testUnusedStmtCond(_ opt: Int?) {
  if let x = opt {}
  // expected-noerror-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}}
  // expected-werror-error@-2 {{value 'x' was defined but never used; consider replacing with boolean test}}
}
