// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend-verify -typecheck %t/test.swift 2>&1 | %update-verify-tests
// RUN: %target-swift-frontend-verify -typecheck %t/test.swift
// RUN: %diff %t/test.swift %t/test.swift.expected

//--- test.swift
func foo() {

//--- test.swift.expected
// expected-note@+1{{to match this opening '{'}}
func foo() {

// expected-error@+1{{expected '}' at end of brace statement}}
