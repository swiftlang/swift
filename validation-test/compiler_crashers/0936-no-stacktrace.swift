// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol C {
protocol B : a {
typealias d {
}
func a<T>() -> String
}
}
struct A : C
