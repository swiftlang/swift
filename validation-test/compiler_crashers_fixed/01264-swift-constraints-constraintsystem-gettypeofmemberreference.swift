// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func f<T>() -> T -> T {
return { x in xol A {
}
struct B<T : A> {
}
struct D : C {
func g<g: e where g.h == f.h> {
}
protocol e {
}
func a(b: Int = 0) {
}
let c = a
