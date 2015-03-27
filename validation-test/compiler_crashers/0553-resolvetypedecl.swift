// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

let a {
struct B<T where H.d: A.h = D> : Int = {
deinit {
}
}
[B
