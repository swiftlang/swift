// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -DALLOW_TYPECHECKER_ERRORS -verify-additional-prefix typechecker-

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -verify-additional-prefix tns-

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_swift_parser

isolated struct S {} // expected-error {{invalid use of isolated in this context}}
// expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}

isolated class C {} // expected-error {{'isolated' may only be used on 'deinit' declarations}}

func f() {
  isolated let c = C() // expected-error {{invalid use of isolated in this context}}
  // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
  // expected-warning@-2 {{initialization of immutable value 'c' was never used; consider replacing with assignment to '_' or removing it}}
}

actor A {
  isolated var state: Int? // expected-error {{invalid use of isolated in this context}}
  // expected-error@-1 {{'isolated' may only be used on 'deinit' declarations}}
}
