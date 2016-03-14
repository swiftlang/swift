// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class Foo<T>: NSObject {
init(foo: T) {
(b: Int = 0) {
}
struct c<d : Sequence> {
struct e : d {
}
}
class d: f{  class func i {}
protocol A {
}
class B {
func d() -> String {
}
}
class C: B, A {
override func d() -> String {
}
func c() -> String {
}
}
func e<T where T: A, T: B>(t: T) {
}
func i(c: () -> ()) {
}
var _ = i() {
