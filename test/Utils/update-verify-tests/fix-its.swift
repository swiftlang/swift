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

