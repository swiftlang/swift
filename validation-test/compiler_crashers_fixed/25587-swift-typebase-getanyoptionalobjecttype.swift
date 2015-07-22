// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

let a{class A{class B{class A{class A{func f<d}}}}enum a{{}enum S<T where T:a{class B
