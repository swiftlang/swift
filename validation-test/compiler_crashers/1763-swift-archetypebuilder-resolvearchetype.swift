// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: asan

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func f: a {
class c<T where T : c<T>
