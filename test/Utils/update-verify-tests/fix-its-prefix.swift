// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -verify-additional-prefix tns- -typecheck %t/test.swift 2>&1 | %update-verify-tests --prefix tns-
// RUN: %target-swift-frontend-verify -verify-additional-prefix tns- -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func customPrefix() {
  let a = 2 // expected-tns-warning{{initialization of immutable value 'a' was never used}} {{1-2=wrong}}
}

//--- test.swift.expected
func customPrefix() {
  let a = 2 // expected-tns-warning{{initialization of immutable value 'a' was never used}} {{3-8=_}}
}

