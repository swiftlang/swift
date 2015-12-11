// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct c{protocol A:a{}protocol a{{}}class P{enum S{struct S<B{struct B{struct X<T:T.a
