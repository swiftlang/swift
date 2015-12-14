// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct S<T where g:a{func a{struct S<T where h:p}class B<b{class A{var b{class c:A
