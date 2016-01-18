// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class A {
}
let c: A? = nil
if c == .b {
}
func c<d {
enum c {
func e
var _ = e
