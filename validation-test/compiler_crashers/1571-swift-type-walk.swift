// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: asan
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class B<T : A {
protocol B : A, y: a {
}
}
class A<T, d: B<T>
