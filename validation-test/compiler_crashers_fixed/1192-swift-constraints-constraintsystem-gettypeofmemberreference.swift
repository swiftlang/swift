// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class Foo<T>: NSObject {
init(foo: T) {
(b: Int = 0) {
}
struct c<d : SequenceType> {
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
