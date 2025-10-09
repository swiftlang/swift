// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/custom_attrs_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/custom_attrs_B.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -verify-ignore-unknown -I %t %s
import custom_attrs_A
import custom_attrs_B

// https://github.com/apple/swift/issues/55912

struct Test {
  @Wrapper var x: Int = 17
  // expected-error@-1 {{'Wrapper' is ambiguous for type lookup in this context}}

  init(@Builder closure: () -> Int) {}
  // expected-error@-1 {{'Builder' is ambiguous for type lookup in this context}}
}

