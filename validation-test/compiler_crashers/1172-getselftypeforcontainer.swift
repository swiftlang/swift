// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<f : b, g : b where f.d == g> {
}
protocol b {
typealias d
: C {
}
typealias C = B
func a(b: I
