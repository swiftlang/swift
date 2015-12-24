// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

private class B<C> {
init(c: C) {
s {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
}
func f() {
}
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
enum A : String {
case b = ""
}
let c: A? = nil
if c == .b {
