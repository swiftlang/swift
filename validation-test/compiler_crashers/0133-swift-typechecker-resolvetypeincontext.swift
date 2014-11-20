// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol f : f {
}
func h<d {
    enum h {
        func e
        var _ = e
    }
}
protocol e {
    e func e()
}
struct h {
    var d: e.h
    func e() {
        d.e()
    }
}
protocol f {
  i []
}
func f<g>() -> (g, g -> g) -> g
