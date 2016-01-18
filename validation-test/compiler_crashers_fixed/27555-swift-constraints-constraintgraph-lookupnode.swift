// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{class a{enum B<T where I:A{class c{class B{let a{class A{let f=a<T:{}}}}var f:a
