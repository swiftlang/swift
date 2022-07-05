// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_isolated~partial.swiftmodule -primary-file %S/Inputs/def_isolated.swift -module-name def_isolated  -disable-availability-checking
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_isolated~partial.swiftmodule -module-name def_isolated -o %t/def_isolated.swiftmodule  -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown  -disable-availability-checking

// REQUIRES: concurrency

import def_isolated

func test(a: A, a2: isolated A, s: S) async {
  await s.f(a: a)
  s.f(a: a) // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to instance method 'f(a:)' from outside of its actor context are implicitly asynchronous}}

  s.f(a: a2)
}
