// RUN: %target-typecheck-verify-swift

// Make sure InlineArray type sugar is disabled by default.
// FIXME: The recovery here really isn't great.
do {
  let _: [3 of Int] // expected-note {{to match this opening '['}}
  // expected-error@-1 4{{expected}}
  // expected-error@-2 4{{consecutive statements on a line must be separated by ';'}}
  // expected-warning@-3 2{{is unused}}
  // expected-error@-4 {{cannot find 'of' in scope}}
  // expected-note@-5 {{add arguments after the type to construct a value of the type}}
  // expected-note@-6 {{use '.self' to reference the type object}}
}
do {
  let _ = [3 of Int]()
  // expected-error@-1 {{cannot call value of non-function type '[Int]'}}
  // expected-error@-2 {{expected ',' separator}}
  // expected-error@-3 {{cannot find 'of' in scope}}
}
