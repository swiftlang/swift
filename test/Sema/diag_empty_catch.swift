// RUN: %target-typecheck-verify-swift

enum MyError: Error { case bad }

func something() throws {}

func testStrictCatch() {
  // 1. Standard Empty Catch -> WARN
  do {
    try something()
  } catch { // expected-warning {{empty catch block silences errors; use 'try?' or 'catch _' to explicitly ignore}}
  }

  // 2. Explicit Ignore -> PASS
  do {
    try something()
  } catch _ {
    // Valid: Explicit intent to ignore
  }

  // 3. Explicit Ignore -> PASS
  do {
    try something()
  } catch {
    _ = error // Valid: User silenced it their own way
  }

  // 4. Other Statements -> PASS
  do {
    try something()
  } catch {
    if true {}
  }

  // 5. Catch All with Explicit Binding -> WARN
  // Even if you bind 'let e', if the body is empty, it's effectively an ignored error.
  do {
    try something()
  } catch let e { // expected-warning {{empty catch block silences errors; use 'try?' or 'catch _' to explicitly ignore}}
    // expected-warning@-1 {{immutable value 'e' was never used; consider replacing with '_' or removing i}}
  }
}

func testCatchChain() {
  // 6. Catch-All in a Chain -> WARN
  do {
    try something()
  } catch MyError.bad {
    print("bad")
  } catch { // expected-warning {{empty catch block silences errors; use 'try?' or 'catch _' to explicitly ignore}}
  }
}
