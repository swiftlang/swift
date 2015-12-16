// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct c {
func e() {
}
let c = a
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
protocol a : a {
d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
func a<
