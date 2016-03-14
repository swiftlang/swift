// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A {
typealias B
}
class C<D> {
init <A: A where A.B == D>(e: A.B) {
P {
}
struct d<f : e, g: e where g.h == f.h> {
}
class A {
class func a() -> String {
struct c {
static let d: String = {
}
}
}
func b<T>(t: s d<c>: NSO
