// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
