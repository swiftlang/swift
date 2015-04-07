// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct A {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
enum A : String {
case b = ""
}
let c: A? = nil
if c == .b {
