// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a {
var _ = i() {
}
func i<j : b, k : d where k.f == j> (n: k) {
}
}
func ^(a: Boolean, Bool) -> Bool {
}
struct d<f : e, g: e where g.h == f.h>
