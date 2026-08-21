// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -verify-additional-prefix check- -typecheck %t/test.swift 2>&1 | %update-verify-tests --prefix check-
// RUN: %target-swift-frontend-verify -verify-additional-prefix check- -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func mixedPrefixesOnOneLine() {
  // Two directives share one comment, with different prefixes. The prefixed
  // one is stale; picking the right directive to rewrite has to survive the
  // prefix filtering.
  a = 2; b = 2 // expected-check-error {{stale}} expected-error {{cannot find 'b' in scope}}
}
//--- test.swift.expected
func mixedPrefixesOnOneLine() {
  // Two directives share one comment, with different prefixes. The prefixed
  // one is stale; picking the right directive to rewrite has to survive the
  // prefix filtering.
  a = 2; b = 2 // expected-check-error {{cannot find 'a' in scope}} expected-error {{cannot find 'b' in scope}}
}
