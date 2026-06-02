// RUN: %target-typecheck-verify-swift

func testUselessIfCondition(_ x: Int) {
  // With else clause: replace the entire `if` with the body of the then branch.

  // expected-warning@+1 {{'if' condition is always true}}{{group-name=UselessConditionalStatement}}{{3-+4:4=_ = 0}}
  if case _ = x {
    _ = 0
  } else {
    _ = 1
  }

  // Multi-line body: each interior line is dedented to match the `if`'s indent.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+3:4=_ = 0\n  _ = 1}}
  if case _ = x {
    _ = 0
    _ = 1
  }

  // Empty body: `fixItReplace` with empty replacement triggers smart whitespace
  // handling that removes the entire pair of lines.

  // expected-warning@+1 {{'if' condition is always true}}{{1-+2:1=}}
  if case _ = x {
  }

  // Same-line body: trim outer whitespace; no newlines in the replacement.

  // expected-warning@+1 {{'if' condition is always true}}{{3-26=_ = 0}}
  if case _ = x { _ = 0 }

  // Blank lines within the body are preserved.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+4:4=_ = 0\n\n  _ = 1}}
  if case _ = x {
    _ = 0

    _ = 1
  }

  // Code on the same line as `{`: the leading statement is preserved and
  // emitted on the same line as the dedented body.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+2:4=_ = 0\n  _ = 1}}
  if case _ = x { _ = 0
    _ = 1
  }

  // Code on the same line as `}`: the trailing statement is preserved.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+2:12=_ = 0\n  _ = 1}}
  if case _ = x {
    _ = 0
    _ = 1 }

  // Nested statement inside the body: dedent strips one indentation level
  // while preserving the relative nesting.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+4:4=if true {\n    _ = 0\n  \}}}
  if case _ = x {
    if true {
      _ = 0
    }
  }

  // `else if` branch: the inner `if` is replaced by the text of its body
  // braces, turning the chain into a plain `else` branch and discarding any
  // now-unreachable trailing branches.

  // expected-warning@+2 {{'if' condition is always true}}{{10-+2:4={\n    _ = 0\n  \}}}
  if false {
  } else if case _ = x {
    _ = 0
  }

  // `else if` with subsequent unreachable branches: those branches are
  // dropped along with the inner `if` header.

  // expected-warning@+2 {{'if' condition is always true}}{{10-+6:4={\n    _ = 0\n  \}}}
  if false {
  } else if case _ = x {
    _ = 0
  } else if x > 0 {
    _ = 1
  } else {
    _ = 2
  }

  // Single-line `else if`: the body braces are extracted intact and replace
  // the inner `if` header.

  // expected-warning@+1 {{'if' condition is always true}}{{21-44={ _ = 0 \}}}
  if false { } else if case _ = x { _ = 0 }

  // Empty `else if` body: extracted text is just `{\n  }`, producing a plain
  // `else { }`.

  // expected-warning@+2 {{'if' condition is always true}}{{10-+1:4={\n  \}}}
  if false {
  } else if case _ = x {
  }

  // `else if` at the end of a longer chain: only the innermost `if` is
  // rewritten; preceding branches are preserved.

  // expected-warning@+4 {{'if' condition is always true}}{{10-+2:4={\n    _ = 0\n  \}}}
  if false {
  } else if x > 0 {
    _ = 99
  } else if case _ = x {
    _ = 0
  }

  // A nested `if` that lives inside an `else { ... }` block (not an
  // `else if` chain) is treated as a regular always-true `if`: the fix-it
  // dedents the body rather than producing brace text. This verifies that
  // the else-if flag is correctly reset when descending into a non-if else.

  if false {
  } else {
    // expected-warning@+1 {{'if' condition is always true}}{{5-+2:6=_ = 0}}
    if case _ = x {
      _ = 0
    }
  }

  // An always-true outer `if` whose else clause contains an `else if` chain:
  // the outer fix-it discards the entire chain along with the if header.

  // expected-warning@+1 {{'if' condition is always true}}{{3-+4:4=_ = 0}}
  if case _ = x {
    _ = 0
  } else if x > 0 {
    _ = 1
  }

  // A labeled `if` is left untouched: no fix-it, since the label may be
  // referenced by `break`/`continue` inside the body.

  // expected-warning@+1 {{'if' condition is always true}}{{none}}
  outer: if case _ = x {
    break outer
  }
}

func testUselessWhile(_ x: Int) {
  // No fix-it for `while`: removing an always-true condition would create an
  // unconditional infinite loop.

  // expected-warning@+1 {{'while' condition is always true}}{{group-name=UselessConditionalStatement}}{{none}}
  while case _ = x {
    _ = 0
    break
  }
  _ = 1
}

func testUselessGuard(_ x: Int) {
  // Multi-line: fix-it removes the entire guard statement (its body is unreachable).

  // expected-warning@+1 {{'guard' condition is always true, body is unreachable}}{{group-name=UselessConditionalStatement}}{{1-+4:1=}}
  guard case _ = x else {
    _ = 0
    return
  }
  _ = 1

  // Same line: fix-it removes the entire line.

  // expected-warning@+1 {{'guard' condition is always true, body is unreachable}}{{1-+1:1=}}
  guard case _ = x else { return }
  _ = 1
}

func testUselessGuardEmptyBody(_ x: Int) {
}
