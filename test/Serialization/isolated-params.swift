// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_isolated~partial.swiftmodule -primary-file %S/Inputs/def_isolated.swift -module-name def_isolated  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_isolated~partial.swiftmodule -module-name def_isolated -o %t/def_isolated.swiftmodule  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

import def_isolated

func test(a: A, a2: isolated A, s: S) async {
  await s.f(a: a)
  s.f(a: a) // expected-error{{actor-isolated instance method 'f(a:)' cannot be called from outside of the actor}} {{3-3=await }}

  s.f(a: a2)
}
