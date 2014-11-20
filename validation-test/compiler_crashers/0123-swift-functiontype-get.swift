// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func g<h>() -> (h, h -> h) -> h {
    f f: ((h, h -> h) -> h)!
    j f
}
protocol f {
    class func j()
}
struct i {
    f d: f.i
    func j() {
        d.j()
    }
}
class g {
    typealias f = f
}
func g(f: Int = k) {
}
let i = g
