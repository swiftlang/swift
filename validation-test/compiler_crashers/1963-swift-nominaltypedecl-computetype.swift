// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
protocol A : a {
self.<Q<T: SequenceType, A = c> Int {
}
func a
}
}
let n1: (z: [Int
extension A
