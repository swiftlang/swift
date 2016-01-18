// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{var b{protocol P{let:BooleanType}}class B<T where B:a{class B<c{let a=c
