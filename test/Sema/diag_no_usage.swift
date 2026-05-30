// RUN: %target-typecheck-verify-swift

// MARK: - Unused literals

func testUnusedLiterals() {
  42
  // expected-warning@-1 {{integer literal is unused}}{{group-name=NoUsage}}
  "hello"
  // expected-warning@-1 {{string literal is unused}}
}

// MARK: - Unused function call result

func returnsInt() -> Int { return 42 }

func testUnusedResult() {
  returnsInt()
  // expected-warning@-1 {{result of call to 'returnsInt()' is unused}}{{group-name=NoUsage}}
}

// MARK: - Unused initializer result

class MyClass {}

func testUnusedInitResult() {
  MyClass()
  // expected-warning@-1 {{result of 'MyClass' initializer is unused}}
}

// MARK: - Unused variable binding

func testUnusedVariable() {
  let x = 5
  // expected-warning@-1 {{initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it}}
}

// MARK: - Unused pattern binding

func testUnusedPatternBinding(_ pair: (Int, String)) {
  switch pair {
  case (let n, _): break // expected-warning {{immutable value 'n' was never used; consider replacing with '_' or removing it}}
  }
}

// MARK: - Unused closure capture

func testUnusedCapture() {
  let value = 42
  let _ = { [value] in 0 }
  // expected-warning@-1 {{capture 'value' was never used}}
}

// MARK: - Unused optional try result

enum SomeError: Error { case problem }
func throwingFunc() throws -> Int { throw SomeError.problem }

func testUnusedOptionalTry() {
  try? throwingFunc()
  // expected-warning@-1 {{result of 'try?' is unused}}
}

// MARK: - Unused value in condition binding

func testUnusedStmtCond(_ opt: Int?) {
  if let x = opt {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}}
}
