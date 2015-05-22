// RUN: not %target-swift-frontend %s -parse


// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol C {
protocol d : a {
init()
}
func a: d
