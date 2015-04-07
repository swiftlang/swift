// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct A {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
return { _, x in return x
func b(c) -> <d>(() -> d) {
}
func a(b: Int = 0) {
}
let c = a
c()
