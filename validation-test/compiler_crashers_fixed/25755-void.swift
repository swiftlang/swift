// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct B{class a{struct S<T where T:T{struct T}}protocol a{{{}}class b{let a}}
