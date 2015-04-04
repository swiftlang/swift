// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A : a {
}
protocol a {
func e: b {
}
func c(() -> Any) {
}
protocol A {
}
typealias d : e: C {
