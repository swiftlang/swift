// A `// }}` with no expansion directive to close is not a directive at all:
// the verifier ignores it. It is punctuation left over from an expansion
// directive that is no longer there, so the updater removes it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo(_ x: Int) {
  // }}
  let a = x // expected-warning{{wrong message}}
}
// }}

//--- test.swift.expected
func foo(_ x: Int) {
  let a = x // expected-warning{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
}

