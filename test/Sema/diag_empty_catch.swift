// RUN: %target-typecheck-verify-swift

enum MyError: Error { case bad }

func something() throws {}

func testStrictCatch() {
  // 1. Standard Empty Catch -> WARN
  do {
    try something()
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
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
  } catch let e {
        // expected-warning@-1 {{immutable value 'e' was never used; consider replacing with '_' or removing i}}}
  }
}

func testCatchChain() {
  // 6. Catch-All in a Chain -> PASS
  do {
    try something()
  } catch MyError.bad {
    print("bad")
  } catch {
        // Valid: implicitly ignore the rest
  }
}
