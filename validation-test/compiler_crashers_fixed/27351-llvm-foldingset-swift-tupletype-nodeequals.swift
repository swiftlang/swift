// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct S<T where g:T{{}struct d{class A{}class c:A{enum S{struct c{class A{struct T
