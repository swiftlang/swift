// RUN: %target-typecheck-verify-swift -swift-version 6
// Checks that the compiler correctly diagnoses ambiguous initializers using
// trailing closures, rather than crashing.
//
// Fixes #85364

struct S1 {  // expected-note {{found this candidate}} \
             // expected-note {{'S1' previously declared here}}
  var c: () -> Void
}

S1 {}  // expected-error {{ambiguous use of 'S1'}}

struct S1 {  // expected-note {{found this candidate}} \
             // expected-error {{invalid redeclaration of 'S1'}}
  func callAsFunction(_: () -> Void) {}
}

struct S2 {
  init() {}  // expected-note {{found this candidate}}

  init(_ block: () -> Void) { block() }  // expected-note {{found this candidate}}

  func callAsFunction(_ block: () -> Void) { block() }
}

S2 {}  // expected-error {{ambiguous use of 'init'}}
