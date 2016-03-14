// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func r<t>() {
    f f {
        i i
    }
}
struct i<o : u> {
    o f: o
}
func r<o>() -> [i<o>] {
    p []
}
class g<t : g> {
}
class g: g {
}
class n : h {
}
typealias h = n
protocol g {
    func i() -> l  func o() -> m {
        q""
    }
}
func j<t k t: g, t: n>(s: t) {
    s.i()
}
protocol r {
}
protocol f : r {
}
protocol i : r {
}
j
