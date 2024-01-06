// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -enable-experimental-feature PreconcurrencyConformances -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -enable-experimental-feature PreconcurrencyConformances -strict-concurrency=targeted -verify-additional-prefix minimal-targeted-
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-sil -o /dev/null -verify -enable-experimental-feature PreconcurrencyConformances -strict-concurrency=complete -verify-additional-prefix complete-tns-

// REQUIRES: concurrency
// REQUIRES: asserts

protocol Q {
}

do {
  class K {}

  struct A : @preconcurrency Q {} // Ok
  struct B : @preconcurrency K {
    // expected-error@-1 {{'preconcurrency' attribute cannot apply to non-protocol type 'K'}}
    var x: @preconcurrency Int
    // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
  }

  typealias T = @preconcurrency Q
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}

  func test(_: @preconcurrency K) {}
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
}

protocol InvalidUseOfPreconcurrencyAttr : @preconcurrency Q {
  // expected-error@-1 {{'preconcurrency' attribute only applies in inheritance clauses}}
}

struct TestPreconcurrencyAttr {}
extension TestPreconcurrencyAttr : @preconcurrency Q { // Ok
}
