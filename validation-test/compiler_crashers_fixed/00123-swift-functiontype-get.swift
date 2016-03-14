// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
