// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{class a{enum S<T:T.B{}}struct E{let a{{{{}}enum T{class B{let:}}
