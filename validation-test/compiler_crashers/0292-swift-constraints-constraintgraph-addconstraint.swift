// RUN: not --crash %swift %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func e(d: Int = 0) {
let f = e
f()
