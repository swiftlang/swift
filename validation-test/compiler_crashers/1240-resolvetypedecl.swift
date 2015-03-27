// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol a {
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
func ^(a: BooleanType, Bool) -> Bool {
}
class a<f : b, g
