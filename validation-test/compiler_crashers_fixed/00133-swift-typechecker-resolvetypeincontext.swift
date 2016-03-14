// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
