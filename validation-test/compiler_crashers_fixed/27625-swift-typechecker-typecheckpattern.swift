// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

let:{class a{class C<f where I:a{func e}}}class S<T{func a<h{func b<T where h.g=a{
