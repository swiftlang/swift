// RUN: %target-typecheck-verify-swift -typo-correction-limit 5

// This is close enough to get typo-correction.
func test_short_and_close() {
  let foo = 4 // expected-note 5 {{did you mean 'foo'?}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
  let _ = fob + 1 // expected-error {{use of unresolved identifier}}
}
