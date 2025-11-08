// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN:     %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {
  let a = 2 // expected-error@+1{{asdf}}
  b = a   // expected-error@+1{{asdf}}
}

func bar() {
  a = 2   // expected-error@+1{{asdf}}
}

//--- test.swift.expected
func foo() {
  // expected-note@+1{{'a' declared here}}
  let a = 2 // expected-error@+1{{cannot find 'b' in scope; did you mean 'a'?}}
  b = a   
}

func bar() {
  // expected-error@+1{{cannot find 'a' in scope}}
  a = 2   
}

