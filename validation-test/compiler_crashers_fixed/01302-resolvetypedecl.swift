// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct c {
var d: b.Type
func e() {
}
}
class a<f : b, g : b where f.d == g> {
}
protocol b {
}
struct c<h : b> : b {
}
func ^(a: BooleanType, Bool) -> Bool {
}
class a {
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a
