// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct l<l : d> : d {
i j i() {
}
}
protocol f {
}
protocol d : f {
func f
