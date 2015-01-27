// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol f {
}
protocol e : f {
}
protocol i {
}
struct c : i {
}
func i<j : j, d : i j d.c == j> (i: d) {
}
func i<c :{
}
let e = f
protocol e : j { func j
