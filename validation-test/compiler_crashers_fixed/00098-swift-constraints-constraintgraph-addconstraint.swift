// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
