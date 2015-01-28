// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts
// XFAIL: asan

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
protocol i : d { func d
