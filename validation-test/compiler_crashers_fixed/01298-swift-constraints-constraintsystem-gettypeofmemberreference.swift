// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A {
typealias B
}
class C<D> {
init <A: A where A.B == D>(e: A.B) {rn ""
struct c {
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
func a(b: Int = 0) {
}
let c = a
