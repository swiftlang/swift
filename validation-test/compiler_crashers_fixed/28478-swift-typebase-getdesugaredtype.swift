// RUN: not %target-swift-frontend %s -typecheck

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var e={class A:A{init()}class A:A
