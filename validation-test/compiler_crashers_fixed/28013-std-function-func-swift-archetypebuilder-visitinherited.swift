// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var d{protocol A{protocol A{{{{}}}}}class S<T{func a<h{func b<T where h.g=a{}}protocol a{func f(t:A
