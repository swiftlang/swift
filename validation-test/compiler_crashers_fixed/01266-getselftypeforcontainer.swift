// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
