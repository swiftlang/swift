// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<T where T:a{struct B{class a{}class d:a{}}class d<a{{}var f:T
