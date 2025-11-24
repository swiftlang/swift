// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
  a = 2;  b = 2; c = 2;
}

func bar() {
  x = 2;  y = 2; z = 2;
  // expected-error@-1{{cannot find 'x' in scope}}
}
//--- test.swift.expected
func foo() {
  // expected-error@+3{{cannot find 'c' in scope}}
  // expected-error@+2{{cannot find 'b' in scope}}
  // expected-error@+1{{cannot find 'a' in scope}}
  a = 2;  b = 2; c = 2;
}

func bar() {
  x = 2;  y = 2; z = 2;
  // expected-error@-1{{cannot find 'x' in scope}}
  // expected-error@-2{{cannot find 'y' in scope}}
  // expected-error@-3{{cannot find 'z' in scope}}
}
