// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var f1: Int -> Int = {
    return $0
 crashes: Int = { x, f in
}(x1, f1)
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
