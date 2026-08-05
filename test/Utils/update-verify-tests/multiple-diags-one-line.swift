// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func wrongMessageInMiddle() {
  // Three directives share one comment; only the middle message is stale.
  // The reported column is what tells the directives apart, so only that one
  // is rewritten and its siblings are left byte-for-byte alone.
  a = 2; b = 2; c = 2 // expected-error {{cannot find 'a' in scope}} expected-error {{stale message}} expected-error {{cannot find 'c' in scope}}
}

func separateComments() {
  // Each directive opens its own comment. The parser must find both, rather
  // than bailing out on the second '//'.
  d = 2; e = 2 // expected-error {{cannot find 'd' in scope}} // expected-error {{stale message}}
}

func everyDirectiveWrong() {
  // Both messages are stale, so the verifier reports two errors against this
  // one line. Each directive must absorb one of them; attributing both to the
  // same directive would take its count negative. The messages land swapped
  // because that is the pairing the verifier itself reports.
  f = 2; g = 2 // expected-error {{stale one}} expected-error {{stale two}}
}

func staleDirectiveFirst() {
  // The stale directive owns the '//' of the comment. Removing it must hand
  // the '//' to the surviving directive, which is written without one.
  h = 2 // expected-error {{never produced}} expected-error {{cannot find 'h' in scope}}
}

func everyDirectiveStale() {
  // Nothing is emitted on this line, so the whole comment goes away.
  let _ = 0 // expected-error {{never produced}} expected-error {{also never produced}}
}

func countOnMultiDirectiveLine() {
  // A count expectation next to a sibling directive: the two 'i' errors are
  // matched by the count, and only the stale sibling is rewritten.
  i = 2; i = 3; j = 2 // expected-error 2{{cannot find 'i' in scope}} expected-error {{stale message}}
}

func fixitOnMultiDirectiveLine() {
  // The fix-it mismatch is reported inside the first directive's fix-it run,
  // so the corrected fix-it must land on that directive and not on its
  // sibling.
  let k = 2; let l = 3 // expected-warning {{initialization of immutable value 'k' was never used}} {{1-2=wrong}} expected-warning {{initialization of immutable value 'l' was never used}}
}

func alreadyCorrect() {
  // Nothing to update here: a multi-directive line that already matches must
  // round-trip with its unusual whitespace intact.
  m = 2; n = 2 //  expected-error{{cannot find 'm' in scope}}     expected-error {{cannot find 'n' in scope}}
}
//--- test.swift.expected
func wrongMessageInMiddle() {
  // Three directives share one comment; only the middle message is stale.
  // The reported column is what tells the directives apart, so only that one
  // is rewritten and its siblings are left byte-for-byte alone.
  a = 2; b = 2; c = 2 // expected-error {{cannot find 'a' in scope}} expected-error {{cannot find 'b' in scope}} expected-error {{cannot find 'c' in scope}}
}

func separateComments() {
  // Each directive opens its own comment. The parser must find both, rather
  // than bailing out on the second '//'.
  d = 2; e = 2 // expected-error {{cannot find 'd' in scope}} // expected-error {{cannot find 'e' in scope}}
}

func everyDirectiveWrong() {
  // Both messages are stale, so the verifier reports two errors against this
  // one line. Each directive must absorb one of them; attributing both to the
  // same directive would take its count negative. The messages land swapped
  // because that is the pairing the verifier itself reports.
  f = 2; g = 2 // expected-error {{cannot find 'g' in scope}} expected-error {{cannot find 'f' in scope}}
}

func staleDirectiveFirst() {
  // The stale directive owns the '//' of the comment. Removing it must hand
  // the '//' to the surviving directive, which is written without one.
  h = 2 // expected-error {{cannot find 'h' in scope}}
}

func everyDirectiveStale() {
  // Nothing is emitted on this line, so the whole comment goes away.
  let _ = 0
}

func countOnMultiDirectiveLine() {
  // A count expectation next to a sibling directive: the two 'i' errors are
  // matched by the count, and only the stale sibling is rewritten.
  i = 2; i = 3; j = 2 // expected-error 2{{cannot find 'i' in scope}} expected-error {{cannot find 'j' in scope}}
}

func fixitOnMultiDirectiveLine() {
  // The fix-it mismatch is reported inside the first directive's fix-it run,
  // so the corrected fix-it must land on that directive and not on its
  // sibling.
  let k = 2; let l = 3 // expected-warning {{initialization of immutable value 'k' was never used}} {{3-8=_}} expected-warning {{initialization of immutable value 'l' was never used}}
}

func alreadyCorrect() {
  // Nothing to update here: a multi-directive line that already matches must
  // round-trip with its unusual whitespace intact.
  m = 2; n = 2 //  expected-error{{cannot find 'm' in scope}}     expected-error {{cannot find 'n' in scope}}
}
