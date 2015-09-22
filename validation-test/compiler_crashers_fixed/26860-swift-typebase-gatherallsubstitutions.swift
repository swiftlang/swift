// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A{{}typealias b:A}class B<T:A{class c<T>:A{let a{{}}let f=a
