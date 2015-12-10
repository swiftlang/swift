// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class d<I : AnyObject, U, AnyObject) {
protocol a {
init()
}
var f = a()
