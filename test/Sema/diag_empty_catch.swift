// RUN: %target-typecheck-verify-swift

func something() throws {}

func testStrictCatch() {
  // 1. Standard Empty -> WARN
  do {
    try something()
  } catch { // expected-warning {{empty catch block silently ignores errors; consider using 'try?' or 'catch _' to explicitly ignore}}
  }

  // 2. Comment Only -> WARN
  do {
    try something()
  } catch { // expected-warning {{empty catch block silently ignores errors; consider using 'try?' or 'catch _' to explicitly ignore}}
    // Comments do not silence warnings
  }

  // 3. Redundant Assignment (Implicit error) -> WARN
  do {
    try something()
  } catch {
    _ = error // expected-warning {{explicit error capture is redundant; use 'catch _' instead}}
  }

  // 4. Redundant Assignment (Explicit 'let e') -> WARN
  do {
    try something()
  } catch let e {
    _ = e // expected-warning {{explicit error capture is redundant; use 'catch _' instead}}
  }

  // 5. Wildcard -> PASS
  do {
    try something()
  } catch _ {
    // This is the correct way to ignore errors
  }
}

// 6. Partial Handling -> PASS
func testPartial() {
  do {
    try something()
  } catch MyError.bad {
    print("bad")
  } catch _ {
    // Correctly ignores everything else
  }
}

enum MyError: Error { case bad }
