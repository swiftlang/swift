// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct j<d : Sequencpe> {
}
func f<d>() -> [j<d>] {
protocol c : b { func b
