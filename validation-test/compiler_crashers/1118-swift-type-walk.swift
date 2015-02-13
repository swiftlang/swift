// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a<T>() {f {
}
class d: f{  class func i {}
func f() {
}
func prefix(with: String) -> <T>(() -> T) -> String {
}
protocol a : a {
}
protocol f {
}
protocol j : f {
}
protocol e : f {
}
protocol i {
}
struct c : i {
}
func i<j : j, d : i j
init(b: c) {
