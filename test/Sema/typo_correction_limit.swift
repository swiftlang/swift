// RUN: %target-typecheck-verify-swift -typo-correction-limit 5

// This is close enough to get typo-correction.
func test_short_and_close() {
  let boop = 4 // expected-note 5 {{did you mean 'boop'?}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
  let _ = bood + 1 // expected-error {{cannot find 'bood' in scope}}
}
