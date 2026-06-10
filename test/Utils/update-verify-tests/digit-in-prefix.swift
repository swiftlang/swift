// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -verify-additional-prefix swift-5- -typecheck %t/test.swift 2>&1 | %update-verify-tests --prefix swift-5-
// RUN: %target-swift-frontend-verify -verify-additional-prefix swift-5- -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
unboundDigit = 1 // expected-swift-5-error{{wrong text}}

//--- test.swift.expected
unboundDigit = 1 // expected-swift-5-error{{cannot find 'unboundDigit' in scope}}

