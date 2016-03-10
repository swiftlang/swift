// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func<{struct c<T{enum S<T{struct A:a protocol a{associatedtype e}struct S<T:A.e
