// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -verify-additional-prefix check- -typecheck %t/test.swift 2>&1 | %update-verify-tests --prefix check-
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift -verify-additional-prefix check- 
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
  a = 2
  // expected-check-error{{foo}}
  // expected-error{{bar}}

  // expected-error@+1{{baz}}
  b = 3
}
//--- test.swift.expected
func foo() {
  // expected-check-error@+1{{cannot find 'a' in scope}}
  a = 2

  // expected-error@+1{{cannot find 'b' in scope}}
  b = 3
}
