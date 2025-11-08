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

func baz() {
  // expected-error@+2{{cannot find 'a' in scope}}
  // expected-error@+1{{cannot find 'a'}}
  let b = a; let c = a; // expected-error{{asdf}}
}

func qux() {
  let b = a; let c = a; // expected-error{{asdf}}
}

func foobar() {
  var b = 1
  b = a; b = a; // expected-error{{asdf}}
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

func baz() {
  // expected-error@+3{{cannot find 'a' in scope}}
  // expected-error@+2{{cannot find 'a'}}
  // expected-note@+1{{'b' declared here}}
  let b = a; let c = a;
}

func qux() {
  // expected-note@+2{{'b' declared here}}
  // expected-error@+1{{cannot find 'a' in scope}}
  let b = a; let c = a; // expected-error{{cannot find 'a' in scope; did you mean 'b'?}}
}

func foobar() {
  // expected-note@+1 2{{'b' declared here}}
  var b = 1
  b = a; b = a; // expected-error 2{{cannot find 'a' in scope; did you mean 'b'?}}
}

