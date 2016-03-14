// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class f<d : d, j : d k d.l == j> {
}
protocol d {
    i l
    i i
}
struct l<l : d> : d {
    i j i() {
        l.i()
    }
}
protocol f {
}
protocol d : f {
