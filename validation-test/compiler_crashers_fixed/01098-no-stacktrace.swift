// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
private class B<C> {
init(c: C) {
s {
func g<U>(h: (A, U) -> U) -> (A, U) -> U {
}
func f() {
}
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
enum A : String {
case b = ""
}
let c: A? = nil
if c == .b {
