// Tests for cross-file location markers in the Swift frontend's `-verify` mode.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/file1.swift %t/file2.swift

//--- file1.swift
let x: Int = "hello" // #marker1
// expected-error@#marker2{{cannot convert value of type '()' to specified type 'String'}}

//--- file2.swift
// expected-error@#marker1 {{cannot convert value of type 'String' to specified type 'Int'}}
let y: String = () // #marker2
