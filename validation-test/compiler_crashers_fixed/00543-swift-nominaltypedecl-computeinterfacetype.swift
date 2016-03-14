// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class A {
struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : n []
}
protocol a {
}
class b: a {
}
func f<T : Boolean>(b: T) {
}
func e() {
