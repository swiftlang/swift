// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
  bar = 2
}
//--- test.swift.expected
func foo() {
  // expected-error@+1{{cannot find 'bar' in scope}}
  bar = 2
}
