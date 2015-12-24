// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var:{[{class a<T where a<I>:a{class A{class a<T{{}class B:A{var b
