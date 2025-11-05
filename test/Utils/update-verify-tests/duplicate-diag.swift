// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
  // expected-error@+1{{cannot find 'a' in scope}}
  a = 2; a = 2;
  b = 2; b = 2;

  // expected-error@+1 3{{cannot find 'c' in scope}}
  c = 2; c = 2;
  // expected-error 3{{asdf}}
}
//--- test.swift.expected
func foo() {
  // expected-error@+1 2{{cannot find 'a' in scope}}
  a = 2; a = 2;
  // expected-error@+1 2{{cannot find 'b' in scope}}
  b = 2; b = 2;

  // expected-error@+1 2{{cannot find 'c' in scope}}
  c = 2; c = 2;
}
