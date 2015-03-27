// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a {
var _ = i() {
}
func i<j : b, k : d where k.f == j> (n: k) {
}
}
func ^(a: BooleanType, Bool) -> Bool {
}
struct d<f : e, g: e where g.h == f.h>
