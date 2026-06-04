// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func wrongFixit() {
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-12=_}}
}

func noActualFixit() {
  // The compiler emits this diagnostic with no fix-it, so the expected
  // fix-it should be removed entirely.
  undefined() // expected-error{{cannot find 'undefined' in scope}} {{1-1=oops}}
}

func noneToFixit() {
  // {{none}} forbids any fix-its; verifier complains and we replace it with
  // the actual fix-it (preserving the {{none}} so the constraint stays).
  let c = 2 // expected-warning{{initialization of immutable value 'c' was never used}} {{none}}
}

@available(*, deprecated, renamed: "bar(example:)")
func foo(b: Int) {}

func unexpectedFixitWithNone() {
  // The expected fix-it list matches one of the actual fix-its, and {{none}}
  // forbids any extras; the compiler emits an additional fix-it. Verifier
  // reports "unexpected fix-it seen" and we widen the list while preserving
  // {{none}}.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{none}}
  foo(b: 1)
}

//--- test.swift.expected
func wrongFixit() {
  let a = 2 // expected-warning{{initialization of immutable value 'a' was never used}} {{3-8=_}}
}

func noActualFixit() {
  // The compiler emits this diagnostic with no fix-it, so the expected
  // fix-it should be removed entirely.
  undefined() // expected-error{{cannot find 'undefined' in scope}}
}

func noneToFixit() {
  // {{none}} forbids any fix-its; verifier complains and we replace it with
  // the actual fix-it (preserving the {{none}} so the constraint stays).
  let c = 2 // expected-warning{{initialization of immutable value 'c' was never used}} {{3-8=_}} {{none}}
}

@available(*, deprecated, renamed: "bar(example:)")
func foo(b: Int) {}

func unexpectedFixitWithNone() {
  // The expected fix-it list matches one of the actual fix-its, and {{none}}
  // forbids any extras; the compiler emits an additional fix-it. Verifier
  // reports "unexpected fix-it seen" and we widen the list while preserving
  // {{none}}.
  // expected-warning@+2 {{'foo(b:)' is deprecated: renamed to 'bar(example:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@+1 {{use 'bar(example:)' instead}} {{3-6=bar}} {{7-8=example}} {{none}}
  foo(b: 1)
}

