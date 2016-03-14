// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c {
var d: b.Type
func e() {
}
}
class a<f : b, g : b where f.d == g> {
}
protocol b {
}
struct c<h : b> : b {
}
func ^(a: Boolean, Bool) -> Bool {
}
class a {
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a
